function [timestamp,lx,ly,rx,ry] = Ba_importRED250(file, nskip, res, missingx, missingy)
% Imports data from Tobii TX300 as returned by Tobii SDK
% res = [xres yres]

dat = readintfile(file, nskip, 7);

timestamp   = dat(:, 1);
lx          = dat(:, 2); %* res(1);
ly          = dat(:, 3); %* res(2);
lv          = dat(:, 6);
rx          = dat(:, 4); %* res(1);
ry          = dat(:, 5); %* res(2);
rv          = dat(:, 7);

% sometimes we have weird peaks where one sample is (very) far outside the
% monitor. Here, count as missing any data that is more than one monitor
% distance outside the monitor.
qMiss = lx<-res(1) | lx>2*res(1) | ly<-res(2) | ly>2*res(2) | lv == 0;
lx(qMiss) = missingx;
ly(qMiss) = missingy;
qMiss = rx<-res(1) | rx>2*res(1) | ry<-res(2) | ry>2*res(2) | rv == 0;
rx(qMiss) = missingx;
ry(qMiss) = missingy;

return