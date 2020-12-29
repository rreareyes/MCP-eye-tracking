nTrials = stim.nUniqCombination;
nBlocks = stim.nBlockPhase;
idx = nan(24, nTrials);
txt = strings(24, nTrials);
weights = strings(24, 4);
probs = stim.uniqProb.';
for iCase = 1:24
    a = round(stim.probFeedback{1, iCase}(:, 6), 4).';
    [tf, idx(iCase, :)] = ismember(a, probs);
    idx(iCase, :) = idx(iCase, :) - 1;
    txt(iCase) = regexprep(num2str(idx(iCase, :)),'\s+',',');
    weights(iCase) = regexprep(num2str(stim.probCase(iCase, :)),'\s+',',');
end

sequence = nan(10, 60);
blocks = [{sequence}, {sequence}, {sequence}, {sequence}];

charBlock = strings(10, 2);

for iBlock = 1:nBlocks
    for iSequence = 1:10
        blocks{iBlock}(iSequence, :) = stim.randTrialSequenceBlock{iSequence, 1}(:, iBlock).'-1;
        charBlock(iSequence, iBlock) = regexprep(num2str(blocks{iBlock}(iSequence, :)),'\s+',',');
    end
end

cuesL = stim.uniqCombination(:, 1).' - 1;
sequenceL = regexprep(num2str(cuesL),'\s+',',');

cuesR = stim.uniqCombination(:, 2).' - 1;
sequenceR = regexprep(num2str(cuesR),'\s+',',');

