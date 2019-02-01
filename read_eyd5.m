%
% write eyd EYEDAT v5 to text file
% 20190201WF - adapted from "ilabConvertCB.m" in ILAB
%
function [outmat] = read_eyd5(full_file, txtout)
        if nargin == 1
           txtout = [];
        else
           if exist(txtout,'file')
              fprintf('ready have txt file %s!\n', txtout)
              outmat = [];
              return
           end
        end


        fid = fopen(full_file,'r','b');
        if fid == -1
            error('bad file');
        end
        
        tmp          = check_line(fid);
        ILAB.vers    = str2num(tmp(9:end));
        ILAB.date    = removeblanks(check_line(fid));
        ILAB.time    = removeblanks(check_line(fid));
        
        % some ASL defines
        ASL_SEGMENTNUM_BYTE     = 808;
        ASL_SEGMENTDESCRIP_BYTE = 874;
        ASL_DATA_START          = 3072;
        
        
        % dump next lines (time and system id)
        tmp = check_line(fid);
        
        tmp = check_line(fid);
        ILAB.acqRate = tmp;
        ILAB.acqRate = str2num(ILAB.acqRate(1:3));
        ILAB.acqIntvl  = 1000/ILAB.acqRate;
        
        data_items = check_line(fid);
        data_items = str2num(data_items(1:2));
        
        % Get Data Item Names
        for i = 1:data_items
            tmp = fgetl(fid);
            % strip off data descriptor
            s = findstr(tmp,'WORD');
            items{i} = deblank(tmp(1:s-1));
        end
        
        % Check if we have external data and mark flags, Horizontal and Vertical
        % Eye Position and pupil diameter
        eyedatacheck = [0 0 0 0 0];
        if strmatch('EXTERNAL_DATA',items)
            eyedatacheck(1) = 1;
        end
        if strmatch('MARK_FLAGS',items)
            eyedatacheck(2) = 1;
        end
        if strmatch('HORZ_EYE_POS',items)
            eyedatacheck(3) = 1;
        end
        if strmatch('VERT_EYE_POS',items)
            eyedatacheck(4) = 1;
        end
        if strmatch('PUPIL_DIAMETER',items)
            eyedatacheck(5) = 1;
        end
        
        % abort if no eye data.
        if sum(eyedatacheck(3:4)) < 2
            fclose(fid);
            error('ASL: No eye data in file.');
        end
        
        % data reading variables
        % discount external data and mark flags
        discflg = sum(eyedatacheck(1:2));
        dataflg = [1 2];
        if eyedatacheck(5) == 1
            dataflg = [dataflg, 4];
        end
        % setup where in the data stream the horz, vert, and pupil data are
        % located. If values for scene number and/or POG magnitude are present then
        % the data items are shifted. If not then they will be 1,2 and 3.
        readflg = [1 2 3];
        checkstart = 0;
        if strmatch('SCENE_NUMBER',items)
            checkstart = checkstart + 1;
        end
        if strmatch('POG_MAGNITUDE',items)
            checkstart = checkstart + 1;
        end
        readflg = readflg + checkstart;
        
        % Set description to empty as this is not used in the eyedat files.
        % ASL sets it to a dummy value
        ILAB.comment = '';
        
        %=========================================================
        % ASL SEGMENT INFORMATION
        % The Segment Information is a list of  zero or more of
        % the following segment descriptors:
        
        % User Segment Descriptor (bytes are 8 bits)		
        %   0xFF    Byte to Start of Segment    Start Time    Stop Time of Segment
        % (1 byte)	       ( 4 bytes )         ( 4 bytes )       ( 4 bytes )
        
        % Pseudo Segment Descriptor
        %   0xFE     Byte to Start of Segment    Start Time
        % (1 byte)	        ( 4 bytes )          ( 4 bytes )
        
        % End of File Segment Descriptor			
        %   0xFD    Byte Offset to Last Byte of  Data + 1 ( file size in bytes )
        % (1 byte)	                   ( 4 bytes )
        
        % Multi-byte values are stored in the file as LSB...MSB.
        % Each user segment descriptor is followed by zero or more
        % pseudo segment descriptors. The last  descriptor in the list
        % is the end of file segment descriptor. A maximum of 169 segment
        % descriptors are possible ( 168 user/pseudo + 1 end of file ).
        % Start and stop times represent the current video field count at
        % the time the segment was started or stopped. If no segment descriptors
        % are present, the file does not contain any eye position data.
        %===========================================================
        
        % go to the segment descriptors
        fseek(fid,ASL_SEGMENTDESCRIP_BYTE,-1);
        segflag   = 1;
        segnum    = 0;
        segbytes  = [];
        endbyte   = [];
        
        while segflag
            % check if this is a segment
            switch fread(fid,1,'uint8')
                case 255
                    % read start of segment in bytes
                    tmp = fread(fid,4,'uint8');
                    tmp = convert_segbyte(tmp);
                    segnum = segnum + 1;
                    segbytes(segnum,1) = tmp;
                    if segnum > 1
                        segbytes(segnum-1,2) = tmp - 1;
                    end                    
                    % move ahead 8 bytes to next marker (ignore time markers)
                    fseek(fid,8,0);
                case 254
                    % ignore pseudosegments, move ahead 8 bytes;
                    fseek(fid,8,0);           
                case 253
                    % get the end of file byte
                    tmp = fread(fid,4,'uint8');
                    segbytes(segnum,2) = convert_segbyte(tmp);
                case 0
                    segflag = 0;
                    if isempty(segbytes)
                        fclose(fid);
                        ILAB = [];
                        ILAB.error = 'ASL: No data in file.';
                        ilabProgressBar('clear')
                        return
                    end
            end
        end   
        
        % get the total file size
        fseek(fid,0,1);
        eof = ftell(fid);
        
        % % move the file marker to the data area
        % % currently ASL always assumes this is at 3072.
        % fseek(fid,ASL_DATA_START,-1);
        
        % -----------------------------------------------------------------------------------
        % EYEDAT consist of variable length records. Although records always contain
        % eye movement variables, external data or mark flags might be missing.
        % Each record is preceeded by a status byte which indicates whether xdat or
        % mark values are in the record. If no xdat or mark flags are present then the record
        % size is (data_items - 2) int16. Thus each record must be parsed individually.
        % Note that the eye data is in big endian format, unlike the segment descriptions
        % which are little endian. The endian changes are historical and rooted 
        % in hardware changes during the history of the software (confirmed by Josh
        % Borah at ASL).
        %
        % status byte table - each of the following represents a bit
        % -----------------------------------------------------------
        % 7 Current record is first record of a user segment
        % 6 Current record is first record of a pseudo segment
        % 5 Current record is last record of a user segment
        % 4 Current record contains an overtime adjustment value (8 bits)
        % 3 Current record contains an external data value (xdat)
        % 2 Current record contains a mark value
        % 1 Unused
        % 0 Unused
        
        % There can be at most 168 segments in an eyedat file.
        % To start we set up a large matrix as file parsing is then much faster.
        % At the end this will be truncated to where there is actual data.
        % -----------------------------------------------------------------------------------
        
        % Calculate the number of bytes per record assuming that none of the
        % records has xdat or mark flags, and ignore the status byte.
        % That way we are sure to overestimate the number of rows needed
        % in the data matrix.
        minbytesperrecord = ((data_items-2)*2);
        numrecords = ceil((eof - 3072)/minbytesperrecord);
        datamat  = zeros(numrecords,4);
        
        % some variables and counters
        j = 0;
        otime_error =[];
        currcode = 0;
        prevcode = 0;
        exitflg  = 0;
        segidx = zeros(segnum,2);
        
        for i = 1:segnum
            exitflg  = 0;
            % move to the start of each user segment
            fseek(fid,segbytes(i,1),-1);
            segidx(i,1) = j+1;

            fprintf('reading EYEDAT 5000 file %d segment\n', i);
            while ftell(fid) < segbytes(i,2)
                
                %if rem(j,100) == 0
                %    fprintf('update %.02f reading EYEDAT 5000 file\n', 100*ftell(fid)/eof);
                %end
                
                j = j+1;
                % read and parse the status byte
                status = fread(fid,1,'uint8');
                if status ~= 0
                    flags = bitget(status,1:8);
                    
                    % End of user segment.
                    if flags(6)
                        exitflg = 1;
                    end
                    if flags(5) %3 overtime adjust
                        otime = fread(fid,1,'uint8');
                        if i >1
                            k = j-segidx(i-1,2);
                        else
                            k = j;
                        end
                        % j is the starting index of the overtime
                        % k = ASL assigns the overtime to the first 
                        % data point after the overtime has ended. 
                        % i is the current segment number and otime  
                        % is the number of overtimes.
                        otime_error = [otime_error; [j k+otime-1 i otime]];
                        % during overtimes the data don't change. Get the
                        % data just before the start of the overtime and
                        % assign that to all the overtime points.
                        % Note: if j == 1 we are at the first point. There
                        % is nothing to change. However, ASL does skip
                        % those number of points.
                        % Note 2: The ASL field index for 5000 files is
                        % 0-based so the datapoint is +1 versus ASL.
                        % So subtract 1 when setting the ASL point number
                        % (k+otime-1)
                        if j > 1
                            tmp = datamat(j-1,:);
                            datamat(j:j+otime-1,:) = tmp(ones(otime,1),:);
                        end
                            % increment j by the length of the overtime.
                            j = j+otime;
                    end
                    if flags(4) %2 xdat_flag
                        % This calculation masks out bit 16 which is only a control bit.
                        currcode = bitand(65536+fread(fid,1,'uint16'),255);
                        if currcode < 0
                            currcode = 0;
                        end
                        
                        % added code to ignore xdats repeated after only 1 time point
                        if currcode ~= prevcode
                            datamat(j,3) = currcode;
                        end
                        prevcode = currcode;
                    end
                    if flags(3) %1 mark_flag
                        % use mark flags as xdat values unless an xdat exists.
                        % if there is an xdat value then dump the mark flag.
                        tmp = fread(fid,1,'uint16');
                        if ~flags(4) %2 if no xdat value
                            % the next lines strange bit of code is to get matlab to internally
                            % convert an ascii char value to that actual number value, i.e., not
                            % just as a string. This is necessary because mark flags
                            % are stored as ascii values and not as numbers.
                            tmpflag = str2num(char(tmp));
                            % This code is necessary because several versions of ASL's
                            % e5win software (prior to version 1.27) stored mark
                            % flags as binary values
                            if isempty(tmpflag)
                                datamat(j,3) = tmp;
                            else
                                datamat(j,3) = tmpflag;
                            end
                        end
                        %mark_flag = 0;
                    end
                end
                % read horiz and vert eye coordinates and pupil size
                a = fread(fid,data_items-discflg,'int16')';
                if length(a) >= (data_items - discflg)
                    datamat(j,dataflg) = a(readflg);
                else
                    h = errordlg({'Reached EOF early.',...
                            'Proceed with caution and examine data carefully.'},...
                        'ILAB Warning', 'modal');
                    uiwait(h);
                end
                if exitflg % end of user segment
                    segidx(i,2) = j;
                    break
                end
            end
        end
        
        fclose(fid);
        if segidx(i,2) == 0
            segidx(i,2) = j;
        end
        datamat = datamat(1:j,:);
        
        % in version 1.31 of the ASL data acquisition software the eye position
        % measurements are multiplied by 10 to increase resolution. We divide by 10
        % here.
        if ILAB.vers >= 1.31
            datamat(:,1:2) = datamat(:,1:2) / 10;
        end
        
%        % setup the data index
%        [datamat, dataidx] = SetUpCodes(datamat, segidx, AP);
%        
%        ILAB.data    = datamat;
%        if size(dataidx,2) == 3
%            ILAB.index      = dataidx;
%        elseif size(dataidx,2) == 2
%            ILAB.index      = dataidx;
%            ILAB.index(:,3) = NaN;
%        else
%            ILAB.index = [1 size(ILAB.data,1)];
%        end
%        ILAB.trials  = size(ILAB.index,1);
%        
%        % write overtime file
%        if ~isempty(otime_error)
%            fn = writeOtime(fname, otime_error);
%            ILAB.private.asl.otime.ErrStr = 'File_line ASL_line Segment Overtime';
%            ILAB.private.asl.otime.errors = otime_error;
%            ILAB.private.asl.otime.fname = fn;
%        else
%            ILAB.private.asl.otime.errors = 0;
%        end        

        % match other eye tracking out files that are like
        % XDAT    pupil_diam      horz_gaze_coord vert_gaze_coord
        % have 
        %  x y  xdat  pupil
        outmat = datamat(:,[3 4 1 2]);

        % fill in 0 xdats with xdat from above
        % xdatidx = 1
        for i=2:length(outmat)
           if outmat(i,1) == 0
              outmat(i,1) = outmat(i-1,1);
           end
        end

        if ~isempty(txtout)
           ofid = fopen(txtout,'w');
           if ofid == -1
              error('cannot open %s to write', txtout)
           end
           fprintf(ofid, 'XDAT\tpupil_diam\thorz_gaze_coord\tvert_gaze_coord\n');
           fprintf(ofid, '%d\t%d\t%d\t%d\n', outmat');
           fclose(ofid);
        end

end

function tmp = check_line(fid)
   % This function checks if tmp is empty and returns only non-empty strings
   % This is necessary because the transfer of EYD 5000 files can result in variable
   % formats depending on whether the transfer is direct pc-disk -> mac, or
   % indirect via ftp. The former results in carriage return/new line while the
   % latter results in carriage return/carriage return. This messes up fgetl.
   
   i = 0;
   while i == 0
       tmp = fgetl(fid);
       if tmp == -1
           error('Reached end-of-file in ASL conversion. This is a problem');
       elseif ~isempty(tmp)
           i = 1;
       end
   end
end

function fn = writeOtime(fname, otime_error)
   % write error file to inform user of overtimes
   fn = [];
   return
   % do write a file
   if ~isempty(otime_error)
       [pth, fn, ext] = fileparts(fname);
       fn = [fn, '_err.txt'];
       fid = fopen(fn,'w');
       if fid < 0
           warning('ILAB warning: Couldn''t write overtime error file')
       else
           fwrite(fid,sprintf('Data lines with overtime errors\n'),'char');
           fprintf(fid,'File_line: %d, ASL_Line: %d, Segment: %d, Overtime: %d\n',otime_error');
           fclose(fid);
       end
   end
end
function segbyte = convert_segbyte(tmp)
  % converts byte information to decimal format. See ASL segment description.
  
  segbyte = hex2dec([dec2hex(tmp(4),2),dec2hex(tmp(3),2),...
          dec2hex(tmp(2),2),dec2hex(tmp(1),2)]);
end

function out = removeblanks(in)
   % remove leading and trailing blanks in a string
   out = fliplr(deblank(fliplr(deblank(in))));
end
