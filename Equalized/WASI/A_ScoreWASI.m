%% WASI
% This script creates a matrix with the final WASI score for every subject
% The first column contains the subject's ID and the second their score.

% It assumes that the input is a text file with a name format as follows:
% baseWASI-XXX-01.txt
% Where XXX stands for the subject ID.

% It also creates a backup matrix with the individual results of every
% subject, containing the data mentioned in the variable "Headers"

% The input for this script is generated in the baseWasi.m function.

% Finally, it calculates mean, std, and median just for reference.

%% Authorship
% Created by Eduardo Rea for project "Heuristic development"
% NLP Lab UMass Amherst
% January 2018
% ramiro.eduardo.rea at gmail dot com

% Feel free to copy, use and/or modify it.

%% Authorship
% Created by for project Gamble fMRI 
%(same version as Multicue Probabilistic Project)
% NLP Lab UMass Amherst
% January 2018

% THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES 
% ARE DISCLAIMED

%% Clear workspace
clc; clear;

%% Set data directories
folder.Root    = pwd;
folder.Data    = fullfile(folder.Root, 'data'); %location of the mat files
folder.Results = fullfile(folder.Root, 'Results'); %where we are saving the output

%% Name for output
file.WASI = fullfile(folder.Results, 'WASI.mat');

%% Get a list of files to process
cd (folder.Data);
file.ListWASI     = dir('*.txt');     % All your WASI text files
file.NameList = {file.ListWASI.name}; % Get the names

%% Create a NAN matrix to fill with data
nSubjects      = length(file.NameList);
wasi.Scores    = zeros(nSubjects,2);
wasi.IndTrials = cell(nSubjects,2);

%% Loop through files to retrieve accuracy
for subjectLoc = 1:nSubjects
    wasi.FileName   = file.NameList{subjectLoc};    % Get the id if the file and convert to string to read in fopen
    wasi.IdFile     = fopen(wasi.FileName,'rt');    % Read the text file
    wasi.TextData   = fgetl(wasi.IdFile);           % Get the string in the file
    wasi.InsertLine = char(split(wasi.TextData, '/r/n')); % Separate every line whenever you find the pattern "/r/n"
    
    %% Loop through the string to get the values of each column
    wasi.TextMatrix = []; % Empty matrix to fill
    
    for dataLines = 5:35                                              % Lines after the original headers
        wasi.InsertTab  = strsplit(wasi.InsertLine(dataLines,:), '/t'); % Separate data into columns
        wasi.TextMatrix = [wasi.TextMatrix; wasi.InsertTab];          % Stack subjects vertically
    end
    
    %% Set headers for every column
    wasi.IndHeaders = {'Problem', 'Stim Onset', 'Correct Answer', 'Subject Response', 'RT', 'Accurate Response',};
    wasi.IndScore   = [wasi.IndHeaders; wasi.TextMatrix];
   
    wasi.resultMatrix = str2double(wasi.TextMatrix);
    wasi.Score   = sum(wasi.resultMatrix(:,6));
    
    %% Get the final WASI score for every subject
    wasi.SubID                = extractBetween(wasi.FileName,'-','-'); %get the ID based on the file name
    wasi.Scores(subjectLoc,1) = str2double(cell2mat(wasi.SubID)); 
    wasi.Scores(subjectLoc,2) = sum(wasi.Score);
    
    %% Create a backup matrix to save all individual responses for all participants
    wasi.IndTrials{subjectLoc,1} = str2double(cell2mat(wasi.SubID)); %subject ID
    wasi.IndTrials{subjectLoc,2} = wasi.IndScore; %results
    
end

%% Get basi descriptives for the sample
wasi.Mean   = mean(wasi.Scores(:,2));
wasi.Std    = std(wasi.Scores(:,2));
wasi.Median = median(wasi.Scores(:,2));

Scores = wasi.Scores;

%% Save the outputs
save(file.WASI, 'Scores')

%% Get back to scritps folder
cd(folder.Root)
