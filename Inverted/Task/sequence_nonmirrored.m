clear all; rng('shuffle');

%% Define classification task parameters

[stimMat,stim] = generate_sequence_nonmirrored; % generate all possible trial sequence

% defines which trials should be included in 1st/2nd set (intermix mirror images)
% use tempComb and wm.seqTrialType (both 240 rows)
stim.nPhase = 1;
stim.nTrialSet = stim.nUniqCombination;
stim.nSetPhase = stim.nCombination/stim.nTrialSet;
stim.nTrialPhase = stim.nSetPhase * stim.nTrialSet;
stim.nBlockPhase = 2;
stim.nTrialBlock = stim.nTrialPhase/stim.nBlockPhase;

%% Randomize Trial Sequence
stim.nRandTrialSeqPhase = 10; % number of randomized sequence per phase
stim.randTrialSequenceBlock = cell(stim.nRandTrialSeqPhase, stim.nPhase);
stim.randTrialSequence = cell(stim.nPhase);

leftcue  = stim.matCombination(:,1);
rightcue = stim.matCombination(:,2);
invcue   = [rightcue, leftcue];
mixedcue = [stim.matCombination(1:2:end, :); invcue(2:2:end, :)];

stim.matCombination = mixedcue;


%% Create mixed cue feedback
for iPhase = 1 : stim.nPhase 
    for iCase = 1:stim.nProbCase
        normalfeed = stim.probFeedback{iPhase, iCase}(:, 1:7);
        invweights = -normalfeed(:, 1:5);
        
        invprob = [];
        
        for iComb = 1:stim.nUniqCombination
            
            invprob(iComb, 1) = 10^invweights(iComb, 5)/(1+10^invweights(iComb, 5)); 
        
        end
        
        invfeed = [invweights, invprob, normalfeed(:, 7)];
        
        mixedfeed = [normalfeed(1:2:end, :); invfeed(2:2:end, :)];
        
        stim.probFeedback{iPhase, iCase} = mixedfeed;

    end
end

%% Create random sets avoiding showing the same stimuli on the same side consecutively
for iP = 1 : stim.nPhase 
    check = {};
    for iT = 1 : stim.nRandTrialSeqPhase        
        while 1
            tempInd   = randperm(stim.nTrialSet);
            tempTrial = mixedcue(tempInd,1:2);
            diffTrial = diff(tempTrial);
            
            if sum(diffTrial(:,1) == 0) == 0 && sum(diffTrial(:,2) == 0) == 0 
                stim.randTrialSequence{iP}(:, iT) = tempInd'; % randomized index: use with stim.matCombination
                check{iT, iP} = tempTrial;
                break;
                
            end
                       
        end
                
        stim.randTrialSequenceBlock{iT, iP} = reshape(stim.randTrialSequence{iP}(:, iT), stim.nTrialBlock, stim.nBlockPhase);
    end
end

%% SAVE 
save('normal_sequence','stimMat','stim');