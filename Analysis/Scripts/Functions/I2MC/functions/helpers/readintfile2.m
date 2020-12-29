function [list] = readintfile2(name, nskip, cols)

% This function reads tab-delimited textfiles. Numbers are converted to
% float. It will not work if anything else than numbers, NaN or Inf are
% included in the file.

fid = fopen(name,'rt');

for p=1:nskip
    fgetl(fid);
end

str  = fread(fid,inf,'*char');
st   = fclose(fid);
list = sscanf(str','%f');

if nargin>=3 
    try
        list = reshape(list,cols,length(list)/cols).';
    catch ME
        fprintf('Matrix is incomplete: %s\n', ME.message);
    end
end