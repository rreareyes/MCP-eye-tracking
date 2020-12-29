
function [stimMat,stim] = generate_sequence_nonmirrored_equalized

% generates trial sequence for MTurk experiment 
% PROBABILISTIC FEEDBACK
% var filenames =
% 0. "stimuli/circle_blue0_line0.jpg"
% 1. "stimuli/circle_blue0_line90.jpg"
% 2. "stimuli/circle_blue1_line0.jpg"
% 3. "stimuli/circle_blue1_line90.jpg"
% 4. "stimuli/square_blue0_line0.jpg"
% 5. "stimuli/square_blue0_line90.jpg"
% 6. "stimuli/square_blue1_line0.jpg"
% 7. "stimuli/square_blue1_line90.jpg"
% 8. "stimuli/circle_red0_line0.jpg"
% 9. "stimuli/circle_red0_line90.jpg"
% 10. "stimuli/circle_red1_line0.jpg" 
% 11. "stimuli/circle_red1_line90.jpg" 
% 12. "stimuli/square_red0_line0.jpg"
% 13. "stimuli/square_red0_line90.jpg"
% 14. "stimuli/square_red1_line0.jpg"
% 15. "stimuli/square_red1_line90.jpg"

% var filenames =
% 0. "stimuli/1_1111.png"
% 1. "stimuli/2_1112.png"
% 2. "stimuli/3_1121.png"
% 3. "stimuli/4_1122.png"
% 4. "stimuli/5_1211.png"
% 5. "stimuli/6_1212.png"
% 6. "stimuli/7_1221.png"
% 7. "stimuli/8_1222.png"
% 8. "stimuli/9_2111.png"
% 9. "stimuli/10_2112.png"
% 10. "stimuli/11_2121.png" 
% 11. "stimuli/12_2122.png" 
% 12. "stimuli/13_2211.png"
% 13. "stimuli/14_2212.png"
% 14. "stimuli/15_2221.png"
% 15. "stimuli/16_2222.png"

% stimulus property based on file names
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

%% Stimulus Build-Up
stim.nCueDimension = 4; % number of cue dimensions
stim.nCueFeature   = 2; % number of cue features per dimension
stim.nCue          = stim.nCueFeature^stim.nCueDimension; % number of compound cues
%stim.orderedCues   = [1 2 3 5 9 4 6  7 11 13 10 8  12 14 15 16]; 
stim.orderedCues   = [1 16 3 4 12 5 10 7 9  8  11 13 6 14 15 2];
%                     [s n s n s n s  n s  n  s  n  s   n  s  n ]
stim.nCueSite      = 2; % number of cue sites

stim.nUniqCombination = nchoosek(stim.nCue,stim.nCueSite); % number of stimulus combinations
stim.uniqCombination  = nchoosek(stim.orderedCues,stim.nCueSite); % matrix of unique combinations (does not include mirror image)
stim.matCombination   = stim.uniqCombination; 
stim.nCombination     = size(stim.matCombination,1);

% Probability assignments for [color; shape; contour; orientation]
% stimMat(:,1) = color; blue = 1, red = 2
% stimMat(:,2) = shape; circle = 1, square = 2
% stimMat(:,3) = contour; white(0) = 1, black(1) = 2
% stimMat(:,4) = orientation; 0deg = 1, 90deg = 2
stim.probDist = [0.5001 0.65 0.8 0.95]; % probabilty distributions 
stim.probCase = perms(stim.probDist); % all possible permutations of cue probability: [color; shape; contour; orientation]
stim.nProbCase = size(stim.probCase,1);
for iP = 1 : stim.nProbCase
    tempCue = [stim.probCase(iP,:); 1-stim.probCase(iP,:)];
    for iC = 1 : stim.nCue
        for iD = 1 : stim.nCueDimension
            stim.matProbCase{iP}(iC,iD) = tempCue(stimMat(iC,iD),iD); % probability assignment for each combination per probCase
        end 
    end
end

% Calculate feedback probability for each combination (total 240 combinations)
for iP = 1 : stim.nProbCase
    stim.probFeedback{iP} = zeros(stim.nCombination,7);
    for iC = 1 : stim.nCombination
        stim.probUniqComb{iP}(iC,1:4) = stim.matProbCase{iP}(stim.matCombination(iC,1),:);
        stim.probUniqComb{iP}(iC,5:8) = stim.matProbCase{iP}(stim.matCombination(iC,2),:);
        stim.probFeedback{iP}(iC,1:4) = stim.probUniqComb{iP}(iC,1:4) - stim.probUniqComb{iP}(iC,5:8);
        stim.probFeedback{iP}(iC,5) = sum(stim.probFeedback{iP}(iC,1:4)); % sum of weights
        stim.probFeedback{iP}(iC,6) = 10^stim.probFeedback{iP}(iC,5)/(1+10^stim.probFeedback{iP}(iC,5)); % probability that LEFT is correct
        stim.probFeedback{iP}(iC,7) = sum(stim.probFeedback{iP}(iC,1:4)~=0); % number of different dimensions
    end    
end

% indexing sum of weights & probability 
% probFeedback10 = integer version of probFeedback
for iP = 1 : stim.nProbCase
    stim.probFeedback10{iP} = round(stim.probFeedback{iP}(:,1:4).*10);
    stim.probFeedback10{iP}(:,5) = round(stim.probFeedback{iP}(:,5).*10);
    stim.probFeedback10{iP}(:,6) = round(stim.probFeedback{iP}(:,6).*10000);    
    sumWeights10 = unique(stim.probFeedback10{iP}(:,5));
    uniqProb10 = unique(stim.probFeedback10{iP}(:,6));
    
    for iC = 1 : stim.nCombination % indexing: corresponds to index of uniqProb
        stim.probFeedback10{iP}(iC,7) = find(stim.probFeedback10{iP}(iC,6) == uniqProb10);
    end
end
stim.sumWeights = sumWeights10./10;
stim.uniqProb = uniqProb10./10000;

