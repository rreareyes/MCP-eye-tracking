clear all; rng('shuffle');

%% Define classification task parameters

[stimMat,stim] = generate_sequence_training; % generate all possible trial sequence

% defines which trials should be included in 1st/2nd set (intermix mirror images)
% use tempComb and wm.seqTrialType (both 240 rows)
stim.nPhase = 3;
stim.nTrialSet = stim.nUniqCombination;
stim.nSetPhase = stim.nCombination/stim.nTrialSet;
stim.nTrialPhase = stim.nSetPhase * stim.nTrialSet;
stim.nBlockPhase = 4;
stim.nTrialBlock = stim.nTrialPhase/stim.nBlockPhase;
tempComb = stim.matCombination; %[L R]
tempComb(:,3) = [repmat([1 2],1,stim.nUniqCombination/2)'; repmat([2 1],1,stim.nUniqCombination/2)']; 
for iS = 1 : 2
    stim.indSet(:,iS) = find(tempComb(:,3) == iS); % index of trials in stim.matCombination
    stim.combSet{iS} = tempComb(stim.indSet(:,iS),:); % stimulus combination sorted in stim.indSet
end

%% Randomize Trial Sequence

stim.nRandTrialSeqPhase = 10; % number of randomized sequence per phase
for iP = 1 : stim.nPhase 
    for iT = 1 : stim.nRandTrialSeqPhase        
        % control for stimulus repetition (not necessary)
        indRandTrial = [];
        for iS = 1 : stim.nSetPhase
            while 1
                tempInd = randperm(stim.nTrialSet);
                tempTrial = stim.combSet{iS}(tempInd,1:2);
                diffTrial = diff(tempTrial);
                if sum(diffTrial(:,1) == 0) == 0 && sum(diffTrial(:,2) == 0) == 0 % avoid stimulus repetition on both L & R
                    indRandTrial(:,iS) = stim.indSet(tempInd,iS); % matches stim.matCombination index
                    break;
                end
            end
        end
        stim.randTrialSequence{iP}(:,iT) = reshape(indRandTrial,stim.nCombination,1); % randomized index: use with stim.matCombination
        stim.randTrialSequenceBlock{iT,iP} = reshape(stim.randTrialSequence{iP}(:,iT),stim.nTrialBlock,stim.nBlockPhase);
    end
end

%% SAVE 
save('training_sequence','stimMat','stim');