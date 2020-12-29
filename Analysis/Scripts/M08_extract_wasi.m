%% WASI
% This script creates a .csv file with the WASI responses for every subject
% It assumes that the input is a text file with a name format as follows:
% baseWASI-XXX-01.txt
% Where XXX stands for the subject ID.

% The input for this script is generated in the baseWasi.m function.

%% Authorship
% Created by Eduardo Rea for project "Multicue Probabilistic"
% NLP Lab UMass Amherst
% January 2018
% ramiro.eduardo.rea at gmail dot com

% Feel free to copy, use and/or modify it.

%% Clear workspace
clc; clear;

%% Set data directories
cd('..')
folder.Root    = pwd;
folder.Data    = fullfile(folder.Root, 'Data', 'WASI'); %location of the mat files
folder.Results = fullfile(folder.Root, 'Results', 'Datasets'); %where we are saving the output
folder.Scripts = fullfile(folder.Root, 'Scripts'); %location of the scripts

%% Name for output
file.WASI = fullfile(folder.Results, 'WASI.csv');

%% Get a list of files to process
cd (folder.Data);
file.List  = dir('*.txt');     % All your WASI text files
file.Names = {file.List.name}; % Get the names

% Define number of subjects and trials
nSubjects = length(file.Names);
nTrials   = 30;

%% Create a NAN matrix to fill with data
group.Data = [];

for iSubject = 1:nSubjects
    % Load data from the test
    subject.FileName   = file.Names{iSubject};         % Get the id if the file and convert to string to read in fopen
    subject.IdFile     = fopen(subject.FileName,'rt'); % Read the text file
    subject.TextVector = fgetl(subject.IdFile);        % Get the string in the file
    fclose(subject.IdFile);
    
    % Separate lines from the text vector
    subject.TextMatrix = char(split(subject.TextVector, '/r/n')); 
    
    % Remove headers
    subject.TextMatrix(1:5, :) = []; 
    
    %% Loop through the string to get the values of each column
    subject.ID = extractBetween(subject.FileName, '-', '-'); 
    subject.TextResults = []; % Empty matrix to fill
    
    if isempty(subject.TextMatrix) 
        
        subject.Data = NaN(nTrials, 6);
        warning(['Empty file for subject ', char(subject.ID), '. Continuing to the next']);
        
    elseif ~isempty(subject.TextMatrix)
        
        % Clean the data
        subject.TextMatrix(end, :) = []; % Remove empty line at the end
        
        if size(subject.TextMatrix, 1) ~= nTrials
            
            subject.Data = NaN(nTrials, 6);        
            warning(['Incomplete file for subject ', char(subject.ID), '. Continuing to the next']);
            
        else
    
            for iTrial = 1:nTrials                                                   
                
                % Separate data into columns
                subject.TextLine    = strsplit(subject.TextMatrix(iTrial, :), '/t'); 
                subject.TextResults = [subject.TextResults; subject.TextLine];       
                
            end

            % Create final dataset from the subject
            subject.Data = str2double(subject.TextResults);
        
        end %End of length check
        
    end %End of file content check
    
    % Add subject ID and cocatenate subjects
    subject.Results = [repmat(str2double(subject.ID), nTrials, 1), subject.Data];
    group.Data = [group.Data; subject.Results];
    
end

%% Save table with results
group.Headers = {'Subject', 'Problem', 'Onset', 'Answer', 'Choice', 'RT', 'Accurate'};
group.Results = array2table(group.Data, 'VariableNames', group.Headers);

writetable(group.Results, file.WASI)

%% Get back to scritps folder
cd(folder.Scripts)
