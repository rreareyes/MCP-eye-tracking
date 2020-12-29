%clear all;

% var filenames =
% 1. "circle_blue0_line0.jpg"
% 2. "circle_blue0_line90.jpg"
% 3. "circle_blue1_line0.jpg"
% 4. "circle_blue1_line90.jpg"
% 5. "square_blue0_line0.jpg"
% 6. "square_blue0_line90.jpg"
% 7. "square_blue1_line0.jpg"
% 8. "square_blue1_line90.jpg"
% 9. "circle_red0_line0.jpg"
% 10. "circle_red0_line90.jpg"
% 11. "circle_red1_line0.jpg" 
% 12. "circle_red1_line90.jpg" 
% 13. "square_red0_line0.jpg"
% 14. "square_red0_line90.jpg"
% 15. "square_red1_line0.jpg"
% 16. "square_red1_line90.jpg"

%% Non compound version

nFile = 16;

filename{1} = '1_1111.png';
filename{2} = '2_1112.png';
filename{3} = '3_1121.png';
filename{4} = '4_1122.png';
filename{5} = '5_1211.png';
filename{6} = '6_1212.png';
filename{7} = '7_1221.png';
filename{8} = '8_1222.png';
filename{9} = '9_2111.png';
filename{10} = '10_2112.png';
filename{11} = '11_2121.png';
filename{12} = '12_2122.png';
filename{13} = '13_2211.png';
filename{14} = '14_2212.png';
filename{15} = '15_2221.png';
filename{16} = '16_2222.png';

for iF = 1 : nFile
    stimJPG{iF} = imread(filename{iF});
end

%% Compound version

% nFile = 16;
% filename{1} = 'circle_blue0_line0.jpg';
% filename{2} = 'circle_blue0_line90.jpg';
% filename{3} = 'circle_blue1_line0.jpg';
% filename{4} = 'circle_blue1_line90.jpg';
% filename{5} = 'square_blue0_line0.jpg';
% filename{6} = 'square_blue0_line90.jpg';
% filename{7} = 'square_blue1_line0.jpg';
% filename{8} = 'square_blue1_line90.jpg';
% filename{9} = 'circle_red0_line0.jpg';
% filename{10} = 'circle_red0_line90.jpg';
% filename{11} = 'circle_red1_line0.jpg';
% filename{12} = 'circle_red1_line90.jpg';
% filename{13} = 'square_red0_line0.jpg';
% filename{14} = 'square_red0_line90.jpg';
% filename{15} = 'square_red1_line0.jpg';
% filename{16} = 'square_red1_line90.jpg';
% 
% for iF = 1 : nFile
%     stimJPG{iF} = imread(filename{iF});
% end

% create stimulus condition matrix (based on filenames)
% stimMat(:,1) = color; blue = 1, red = 2
% stimMat(:,2) = shape; circle = 1, square = 2
% stimMat(:,3) = contour; white(0) = 1, black(1) = 2
% stimMat(:,4) = orientation; 0deg = 1, 90deg = 2
stimMat(1,:) = [1 1 1 1]; 
stimMat(2,:) = [1 1 1 2]; 
stimMat(3,:) = [1 1 2 1]; 
stimMat(4,:) = [1 1 2 2]; 
stimMat(5,:) = [1 2 1 1]; 
stimMat(6,:) = [1 2 1 2]; 
stimMat(7,:) = [1 2 2 1]; 
stimMat(8,:) = [1 2 2 2]; 
stimMat(9,:) = [2 1 1 1];
stimMat(10,:) = [2 1 1 2]; 
stimMat(11,:) = [2 1 2 1]; 
stimMat(12,:) = [2 1 2 2]; 
stimMat(13,:) = [2 2 1 1]; 
stimMat(14,:) = [2 2 1 2]; 
stimMat(15,:) = [2 2 2 1]; 
stimMat(16,:) = [2 2 2 2]; 

save('stim_eyetracker.mat','stimJPG','stimMat');