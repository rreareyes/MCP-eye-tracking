%% Clear workspace
clear; clc

%% Base directories
cd('..')
folder.Root    = pwd;
folder.Scripts = fullfile(folder.Root, 'Scripts');
folder.Data    = fullfile(folder.Root, 'Data'); 
folder.Results = fullfile(folder.Root, 'Results', 'Datasets');

file.Output   = fullfile(folder.Results, 'responses_dataset.csv');
file.Stim     = fullfile(folder.Data, 'Reference', 'stim_eyetracker.mat');
file.Sequence = fullfile(folder.Data, 'Reference', 'normal_sequence.mat');

%% Get number of files per phase
file.AllList = dir(fullfile(folder.Data, 'Responses', '*.mat'));
nFiles = length(file.AllList);

for iFile = 1:nFiles
    
    group.Names{iFile, 1} = file.AllList(iFile).name(1:3);
    group.IDs = unique(group.Names);
    
end

nSubjects = length(group.IDs);

name.Phases = {'TP', 'NP', 'EP'};
nPhases = length(name.Phases);

%% Get 16 combinations from stim file
load(file.Stim)
load(file.Sequence, 'stim')

%% Loop across subjects
% Preallocate the final matrix
group.DataSet = []; 

for iSubject = 1:nSubjects
    subject.Number = group.IDs{iSubject};
    subject.ID = repmat(str2double(subject.Number), 480, 1);
            
    if str2double(subject.Number) < 500
        
        subject.Experiment = ones(480, 1);
        
    elseif str2double(subject.Number) > 500
        
        subject.Experiment = repmat(2, 480, 1);
        
    end
    
    subject.PhaseResponses = [];
    subject.PhaseRT = [];
    subject.PhaseRank = [];
    subject.PhaseID = [];
    subject.BlockID = [];
    subject.Environment = [];
    subject.PhaseSequence = [];
    subject.CombLeft = [];
    subject.CombRight = [];
    subject.PhaseStates = [];

    %% Loop through all the phases
    for iPhase = 1:nPhases
      
        subject.Phase = name.Phases{iPhase};
      
        subject.ParametersFile = dir(fullfile(folder.Data, 'Responses', [subject.Number '_expParameters_' subject.Phase '.mat']));
        subject.ParametersPath = fullfile(subject.ParametersFile.folder, subject.ParametersFile.name);

        load(subject.ParametersPath, 'param');
        
        param.probCase(:, param.probCase==0.5) = 0.5001;

        subject.BlockList = dir(fullfile(folder.Data, 'Responses', [subject.Number '_' subject.Phase '_Block*.mat']));
        nBlocks = length(subject.BlockList);

        subject.Weights = param.probFeedback{1, 1}(:, 1:4);
        subject.Probs   = param.probFeedback{1, 1}(:, 5:7);
        
        nTrials = length(subject.Weights);

        subject.PhaseNumber = repmat(iPhase, nTrials, 1);

        subject.BlockResponses = [];
        subject.BlockRT = [];
        subject.Block = [];
        subject.BlockSequence = [];
        subject.BlockCombLeft = [];
        subject.BlockCombRight = [];
        subject.BlockStates = [];
        
        if iPhase == 1
            [subject.SortedCase, subject.SortIndex] = sort(param.probCase, 'descend');
            subject.Ranks = 1:4;
            subject.Ranks(subject.SortIndex) = subject.Ranks;
        end
        
        subject.CueRank = repmat(subject.Ranks, nTrials, 1);
        
        subject.SortedWeights = subject.Weights(:, subject.SortIndex);
        
        subject.Setup = [subject.Weights, subject.SortedWeights, subject.Probs];
        
        %% Loop though the blocks within a phase
        for iBlock = 1:nBlocks
          
            % Add a block identifier
            subject.BlockNumber = repmat(iBlock, (nTrials / nBlocks), 1);
            subject.Block = [subject.Block; subject.BlockNumber];
            
            % Get file to load
            subject.BlockFile = fullfile(subject.BlockList(iBlock).folder, subject.BlockList(iBlock).name);
            load(subject.BlockFile, 'dE')
          
            % Obtain the RT and Response
            subject.BlockResponses = [subject.BlockResponses; dE.keyResp];
            subject.BlockRT        = [subject.BlockRT; dE.RT];
            
            % Get the trial sequence id
            subject.BlockSequence = [subject.BlockSequence; param.randTrialSequenceBlock{1}(:, iBlock)];     
            
            % Get combination presented
            subject.BlockCombLeft  = param.matCombinationL{1, 1}(:, iBlock); 
            subject.BlockCombRight = param.matCombinationR{1, 1}(:, iBlock); 
            
            subject.StatesLeft = [];
            subject.StatesRight = [];
            
            for iTrial = 1:param.nTrialBlock
    
                subject.StatesLeft(iTrial, :) = stimMat(subject.BlockCombLeft(iTrial), subject.SortIndex);
                subject.StatesRight(iTrial, :) = stimMat(subject.BlockCombRight(iTrial), subject.SortIndex);

            end
            
%             subject.StatesLeft(subject.StatesLeft == 2) = 0;
%             subject.StatesRight(subject.StatesRight == 2) = 0;
            
            subject.CueStates = subject.StatesLeft - subject.StatesRight;
            
            subject.States = [subject.StatesLeft, subject.StatesRight];
            
            subject.BlockStates = [subject.BlockStates; subject.States];
                              
        end %block loop
        
        %% Create phase matrices
        subject.PhaseResponses = [subject.PhaseResponses; subject.BlockResponses];
      
     	subject.PhaseRT = [subject.PhaseRT; subject.BlockRT];
      
        subject.PhaseID = [subject.PhaseID; subject.PhaseNumber];
      
        subject.BlockID = [subject.BlockID; subject.Block];
      
        subject.Environment = [subject.Environment; subject.Setup];
        
        subject.PhaseRank = [subject.PhaseRank; subject.CueRank];
        
        subject.PhaseSequence = [subject.PhaseSequence; subject.BlockSequence];
        
        subject.CombLeft = [subject.CombLeft; subject.BlockCombLeft];
        
        subject.CombRight = [subject.CombRight; subject.BlockCombRight];
        
        subject.PhaseStates = [subject.PhaseStates; subject.BlockStates];
            
    end %phase loop
    
    
    %% Create subject matrix
    subject.DataSet = [subject.ID, subject.Experiment, subject.PhaseID, subject.BlockID, subject.PhaseSequence, subject.Environment, subject.PhaseResponses, subject.PhaseRT, subject.PhaseRank, subject.PhaseStates];
  
    %% Merge all subjects into final dataset
    group.DataSet = [group.DataSet; subject.DataSet];
  
end %subject loop

% Create variable names and save the data into a csv file to work on R
data.VariableNames = {'subject', 'experiment', 'phase', 'block', 'seq_id', ...
                      'weight1', 'weight2', 'weight3', 'weight4', ...
                      'sorted1', 'sorted2', 'sorted3', 'sorted4', ...
                      'evidence', 'p_left', 'n_different', ...
                      'response', 'rt',  ...
                      'rank_cue1', 'rank_cue2', 'rank_cue3', 'rank_cue4', ...
                      'c1_left', 'c2_left','c3_left', 'c4_left', ...
                      'c1_right', 'c2_right', 'c3_right', 'c4_right'};
                  
data.Table = array2table(group.DataSet, 'VariableNames', data.VariableNames);
writetable(data.Table, file.Output)

%% Return to script folder
cd(folder.Scripts)