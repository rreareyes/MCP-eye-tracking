%% Logistic Regression and Variational Bayes

% This script performs a logistic regression in the dataset from the
% Summary file generated in the script "Extract_Behavioral_Data".

% The model variables are the 4 different cues (4 regressors), which are
% coded as 1 when the cue state A was present, and 0 for cue state B. The
% outcome was whether the participant chose left (1) or right (0). The
% regression weights from this are used to calculate the subjective cue
% weights by getting the log10(e^beta), and are stored in the variable
% betaLog10..

% The regression weights from the first phase (learning) are used to
% estimate hyperpriors for the variational bayesian inference.

% The different decision models are constructed and compared against the
% optimal model 15 (considering the 4 cues). Model 16 corresponds to 
% Take-the-best heuristic (Gigerenzer & Gaissmaier, 2011).

% The variationa Bayes then calculates the bayes factor (BF) for every model
% given the regression weights from the logistic regression, and determines
% the winning model as the one that has the higher BF as long as it is larger 
% than 3. If it doesn't supperate this threshold, the model 15 (H0) is
% determined as the winning model. It uses the functions for variational 
% bayesian inference for linear regression, developed by Jan Drugowitsch 
% (2013).

% It saves the output from the logistic regression and from the variational
% analysis separately as structures.

% It also generates plots for the subjective cue weights from the group and
% for the exceedance probability of each model, with and without
% considering informative priors, which are stored in the variables xp and
% xpPrior, respectively. The exceedance plot creates a fake plot to show
% the different cue usage per model just for visualization.

%% Authorship
% Modified by Eduardo Rea for project "Heuristic development"
% From Hannah Oh et al., 2016
% NLP Lab UMass Amherst
% January 2018

%% Clear the work space
clear; clc

%% Base directories
cd('..')
folder.Root     = pwd;
folder.Scripts  = fullfile(folder.Root, 'Scripts');
folder.Behavior = fullfile(folder.Root, 'Behavior', 'Data'); 
folder.Results  = fullfile(folder.Root, 'Behavior', 'Results');

file.WinModel      = fullfile(folder.Results, 'win_model.xls');
file.WinModelPrior = fullfile(folder.Results, 'win_model_prior.xls');

file.Exceedance      = fullfile(folder.Results, 'exceedance.xls');
file.ExceedancePrior = fullfile(folder.Results, 'exceedance_prior.xls');

%% Set paths to files
file.Behavior = fullfile(folder.Results, 'clean_dataset.xls');

%% Load behavior data
data.Raw  = xlsread(file.Behavior);
data.Base = data.Raw(:, [1, 37, 2, 30:33, 16, 14]); % ID, group, phase, state 1-4, response, p_left

%% Remove trials with equal prob left vs right
data.NonRandom = data.Base(data.Base(:, 8) ~= 0.5, :);

%% Remove NaN responses (missed trials)
data.NonRandom(isnan(data.NonRandom(:,8)), :) = [];

%% Set parameters from the experiment
data.SubjID       = unique(data.NonRandom(:, 1)); 
data.Group        = unique(data.NonRandom(:, 1:2), 'rows'); % quartiles
data.Phase        = unique(data.NonRandom(:, 3)); % [TP, T1, T2] 
data.nCues        = 4;
data.respLocation = data.nCues + 1;
data.idealObs     = 15;
data.cuesUsed     = [1 2 3 4];

%% Get the number of participants processed
data.nSubj  = length(data.SubjID);
data.nPhase = length(data.Phase); 
data.nGroup = 4;

%% LOGISTIC REGRESSION
% Loop through all the subjects and perform logistic regression in the data
% from each phase separately.

% Increase the number of iterations
opts = statset('glmfit');
opts.MaxIter = 100; % default value for glmfit is 100.

for iSubject = 1:data.nSubj
    
    for iPhase = 1 : data.nPhase
        
        % Get Subject data for each phase
        logRegData.Base = data.NonRandom(data.NonRandom(:, 1) == data.SubjID(iSubject) & data.NonRandom(:, 3) == data.Phase(iPhase), :);
        
        nTrials = length(logRegData.Base);
        
        % Build matrices for logistic regression
        logRegData.Regressors{iSubject, iPhase} = logRegData.Base(:, 4:7);
        logRegData.Outcome{iSubject, iPhase}    = logRegData.Base(:, 8);
                
        % Run logistic regression
        % Note: Here we are using the responses as 1=left and 0=right.
        % If we wanted to keep the original labeling 1=left and -1=right,
        % we should add a column of ones in the outcome in the following
        % way: [logRegData.Outcome ones(size(logRegData.Outcome)] which
        % would stand for the number of trials in that row (always 1)
        [b,dev,stats] = glmfit(logRegData.Regressors{iSubject, iPhase}, logRegData.Outcome{iSubject, iPhase}, 'binomial','link','logit', 'options', opts);
        
        % Create matrices for each subject
        logRegData.beta{iPhase}(:, iSubject) = b; % [bias; cue0.95; cue0.8; cue0.65]
        logData.dev{iPhase}(iSubject)        = dev;
        logRegData.stats{iSubject,iPhase}    = stats;

        % Obtain the subjective cue weights for each participant
        logRegData.betaLog10{iPhase}(:,iSubject) = log10(exp(b)); % transform beta weights to log base 10
        
        % Clear values to avoid issues with overwriting
        clear b dev dev stats
        
    end
end

%% Estimate hyperpriors for variational bayesian inference from the Learning Phase
testPhase = 1; % Define learning phase
tempAll   = reshape(logRegData.beta{testPhase}, 1, []); %a single row array with 5(regressors)*nSubjects columns

logRegData.betaLogAvg   = [mean(tempAll), var(tempAll)];
logRegData.betaPrior(1) = logRegData.betaLogAvg(testPhase, 1)^2 / logRegData.betaLogAvg(testPhase, 2);     % a0 = mu^2/sigma^2
logRegData.betaPrior(2) = logRegData.betaLogAvg(testPhase, 1) / logRegData.betaLogAvg(testPhase, 2); % b0 = mu/sigma^2    

%% MODEL COMPARISON USING VARIATIONAL BAYES
%% Model construction
nC = 1;
for iN = 1 : data.nCues % all possible combinations
    indCue = nchoosek(data.cuesUsed,iN);
    
    for iC = 1 : size(indCue,1)
        bayesianModelData.indCueModel{nC} = indCue(iC,:);
        nC = nC + 1;
    end
    
end

%% Create alternative models
bayesianModelData.indCueModel{nC} = data.cuesUsed; % take-the-best
bayesianModelData.indCueModel{17} = [1, 2, 3]; %lazy ttb (3 cues)
bayesianModelData.indCueModel{18} = [1, 2]; %lazy ttb (2 cues)
bayesianModelData.indCueModel{19} = 1; % Tallying
bayesianModelData.indCueModel{20} = 1; % Tallying using 3 cues
bayesianModelData.indCueModel{21} = 1; % Tallying using 2 cues

normal_models  = 1:15;
ttb            = 16;
lazy_ttb_3     = 17;
lazy_ttb_2     = 18;
tallying_4     = 19;
tallying_3     = 20;
tallying_2     = 21;


%% Get number of models
bayesianModelData.nModel = size(bayesianModelData.indCueModel,2); % total number of models

%% Variational Bayes
bayesianModelData.winModel = NaN(data.nPhase,data.nSubj);
bayesianModelData.winModelPrior = NaN(data.nPhase,data.nSubj);

for iPhase = 1 : data.nPhase
    
    for iSubject = 1 : data.nSubj
        bayesianModelData.w{iSubject, iPhase}          = zeros(bayesianModelData.nModel, 5);
        bayesianModelData.w10{iSubject, iPhase}        = zeros(bayesianModelData.nModel, 5);
        bayesianModelData.diagV{iSubject, iPhase}      = zeros(bayesianModelData.nModel, 5);
        bayesianModelData.wPrior{iSubject, iPhase}     = zeros(bayesianModelData.nModel, 5);
        bayesianModelData.w10Prior{iSubject, iPhase}   = zeros(bayesianModelData.nModel, 5);
        bayesianModelData.diagVPrior{iSubject, iPhase} = zeros(bayesianModelData.nModel, 5);
        
        bayesianModelData.Outcome{iSubject,iPhase} = logRegData.Outcome{iSubject, iPhase};
        %logRegData.Regressors{iSubject, iPhase} = logRegData.Regressors{iSubject, iPhase}.* [0.9, 0.6, 0.3, 0.0001];
        %this line was only to account for some weighting when creating the
        %models, not sure if this approach is even valid/meaningful anyway
        
        for iModel = 1 : bayesianModelData.nModel
            
            % Create input matrix for every model
            % Models pointing to usage of clues all over the task
            if ismember(iModel, normal_models)
                
                bayesianModelData.xModel{iSubject,iModel,iPhase} = logRegData.Regressors{iSubject, iPhase}(:, bayesianModelData.indCueModel{iModel});

            % Models using sequential sampling
            % TTB
            elseif ismember(iModel, ttb)
                
                bayesianModelData.xModel{iSubject, iModel, iPhase} = zeros(size(logRegData.Regressors{iSubject, iPhase}));

                
                for iTrial = 1 : size(logRegData.Regressors{iSubject, iPhase}, 1)
                    temp1 = find(logRegData.Regressors{iSubject, iPhase}(iTrial, :) ~= 0, 1); %first non zero element from the regressor matrix
                    bayesianModelData.xModel{iSubject, iModel, iPhase}(iTrial,temp1) = logRegData.Regressors{iSubject, iPhase}(iTrial, temp1);
                end
            
            % Lazy TTB (3 cues inspected)
            elseif ismember(iModel, lazy_ttb_3) 
                
                bayesianModelData.xModel{iSubject, iModel, iPhase} = zeros(size(logRegData.Regressors{iSubject, iPhase}));
                
                for iTrial = 1 : size(logRegData.Regressors{iSubject, iPhase}, 1)
                    temp2 = find(logRegData.Regressors{iSubject, iPhase}(iTrial, :) ~= 0, 1); %first non zero element from the regressor matrix
                    bayesianModelData.xModel{iSubject, iModel, iPhase}(iTrial, temp2) = logRegData.Regressors{iSubject, iPhase}(iTrial, temp2);
                end
                
                bayesianModelData.xModel{iSubject, iModel, iPhase}(:, 4) = [];
                
            % Lazy TTB (2 cues inspected)
            elseif ismember(iModel, lazy_ttb_2)
                
                bayesianModelData.xModel{iSubject, iModel, iPhase} = zeros(size(logRegData.Regressors{iSubject, iPhase}));
                
                for iTrial = 1 : size(logRegData.Regressors{iSubject, iPhase}, 1)
                    temp2 = find(logRegData.Regressors{iSubject, iPhase}(iTrial, :) ~= 0, 1); %first non zero element from the regressor matrix
                    bayesianModelData.xModel{iSubject, iModel, iPhase}(iTrial, temp2) = logRegData.Regressors{iSubject, iPhase}(iTrial, temp2);
                end
                
                bayesianModelData.xModel{iSubject, iModel, iPhase}(:, 3:4) = [];
               
            % Tallying
            elseif ismember(iModel, tallying_4)
                
                bayesianModelData.xModel{iSubject, iModel, iPhase} = zeros(size(logRegData.Regressors{iSubject, iPhase}(:, bayesianModelData.indCueModel{iModel})));              
                
                for iTrial = 1 : size(logRegData.Regressors{iSubject, iPhase}, 1)
                    
                    bayesianModelData.xModel{iSubject, iModel, iPhase}(iTrial, 1) = sum(logRegData.Regressors{iSubject, iPhase}(iTrial, :));
                    
                end
                
            % Tallying 1:3 
            elseif ismember(iModel, tallying_3)
                
                bayesianModelData.xModel{iSubject, iModel, iPhase} = zeros(size(logRegData.Regressors{iSubject, iPhase}(:, bayesianModelData.indCueModel{iModel})));              
                
                for iTrial = 1 : size(logRegData.Regressors{iSubject, iPhase}, 1)
                    
                    bayesianModelData.xModel{iSubject, iModel, iPhase}(iTrial, 1) = sum(logRegData.Regressors{iSubject, iPhase}(iTrial, 1:3));
                    
                end
                            
            % Tallying 1:2
            elseif ismember(iModel, tallying_2)
                
                bayesianModelData.xModel{iSubject, iModel, iPhase} = zeros(size(logRegData.Regressors{iSubject, iPhase}(:, bayesianModelData.indCueModel{iModel})));              
                
                for iTrial = 1 : size(logRegData.Regressors{iSubject, iPhase}, 1)
                    
                    bayesianModelData.xModel{iSubject, iModel, iPhase}(iTrial, 1) = sum(logRegData.Regressors{iSubject, iPhase}(iTrial, 1:2));
                    
                end
                            
            end
            
            % Run variational bayes using noninformative priors
            [w, V, invV, logdetV, E_a, L] = bayes_logit_fit([ones(size(bayesianModelData.xModel{iSubject, iModel, iPhase}, 1), 1) bayesianModelData.xModel{iSubject, iModel, iPhase}], bayesianModelData.Outcome{iSubject, iPhase});
            
            bayesianModelData.w{iSubject, iPhase}(iModel, [1 bayesianModelData.indCueModel{iModel}+1])     = w; % cue weights
            bayesianModelData.w10{iSubject, iPhase}(iModel, [1 bayesianModelData.indCueModel{iModel}+1])   = log10(exp(w));
            bayesianModelData.diagV{iSubject, iPhase}(iModel, [1 bayesianModelData.indCueModel{iModel}+1]) = diag(V); % variance of w
            bayesianModelData.E_a{iPhase}(iSubject, iModel)                                                = E_a;
            bayesianModelData.L{iPhase}(iSubject, iModel)                                                  = L; % log-likelihood of the data given the model (lower bound); larger L = better fit
            
            % Using priors from logistic regression
            [w2, V2, invV2, logdetV2, E_a2, L2] = bayes_logit_fit_prior([ones(size(bayesianModelData.xModel{iSubject, iModel, iPhase}, 1) ,1) bayesianModelData.xModel{iSubject, iModel, iPhase}], bayesianModelData.Outcome{iSubject, iPhase}, logRegData.betaPrior(1), logRegData.betaPrior(2));
            
            bayesianModelData.wPrior{iSubject, iPhase}(iModel, [1 bayesianModelData.indCueModel{iModel} + 1])     = w2; % cue weights
            bayesianModelData.w10Prior{iSubject, iPhase}(iModel, [1 bayesianModelData.indCueModel{iModel} + 1])   = log10(exp(w2));
            bayesianModelData.diagVPrior{iSubject, iPhase}(iModel, [1 bayesianModelData.indCueModel{iModel} + 1]) = diag(V2); % variance of w
            bayesianModelData.E_aPrior{iPhase}(iSubject, iModel)                                              = E_a2;
            bayesianModelData.LPrior{iPhase}(iSubject, iModel)                                                = L2; % log-likelihood of the data given the model (lower bound); larger L = better fit
            
            clear w V invV logdetV E_a L w2 V2 invV2 logdetV2 E_a2 L2
            
        end
        
        %% Weights using the optimal model (model 15)
        bayesianModelData.optW10{iPhase}(iSubject, :)      = bayesianModelData.w10{iSubject, iPhase}(data.idealObs, :); % optimal weights
        bayesianModelData.optW10Prior{iPhase}(iSubject, :) = bayesianModelData.w10Prior{iSubject, iPhase}(data.idealObs, :); % optimal weights using semi-informative prior
        
        %% Adjust log-likihood based on the optimal model
        bayesianModelData.adjL{iPhase}(iSubject, :)      = bayesianModelData.L{iPhase}(iSubject, :) - bayesianModelData.L{iPhase}(iSubject,data.idealObs);
        bayesianModelData.adjLPrior{iPhase}(iSubject, :) = bayesianModelData.LPrior{iPhase}(iSubject, :) - bayesianModelData.LPrior{iPhase}(iSubject,data.idealObs); 
        
        %% Calculate bayes factor
        bayesianModelData.BF{iPhase}(iSubject, :)      = exp(bayesianModelData.adjL{iPhase}(iSubject,:));
        bayesianModelData.BFPrior{iPhase}(iSubject, :) = exp(bayesianModelData.adjLPrior{iPhase}(iSubject,:));
        
        %% Find the winning model (BF > 3)
        [wm,iwm] = sort(bayesianModelData.BF{iPhase}(iSubject,:),'descend'); %iwm tells us the position (i.e. the model); wm is the BF from that model
        if wm(1) > 3
            bayesianModelData.winModel(iPhase,iSubject) = iwm(1);
            
        else % winning model is the optimal model
            bayesianModelData.winModel(iPhase,iSubject) = data.idealObs; 
            
        end
            
        [wmPrior,iwmPrior] = sort(bayesianModelData.BFPrior{iPhase}(iSubject,:),'descend');
        
        if wmPrior(1) > 3
            bayesianModelData.winModelPrior(iPhase,iSubject) = iwmPrior(1); 
            
        else % winning model is the optimal model
            bayesianModelData.winModelPrior(iPhase,iSubject) = data.idealObs; 
            
        end
        
    end
    
    for iGroup = 1:data.nGroup
        %% Average cue weight per group
        quartileModel.optW10Mean{iGroup, iPhase}      = mean(bayesianModelData.optW10{iPhase}(data.Group(:, 2) == iGroup, :));
        quartileModel.optW10SEM{iGroup, iPhase}       = std(bayesianModelData.optW10{iPhase}(data.Group(:, 2) == iGroup, :)) / sqrt(data.nSubj);
        quartileModel.optW10MeanPrior{iGroup, iPhase} = mean(bayesianModelData.optW10Prior{iPhase}(data.Group(:, 2) == iGroup, :));
        quartileModel.optW10SEMPrior{iGroup, iPhase}  = std(bayesianModelData.optW10Prior{iPhase}(data.Group(:, 2) == iGroup, :)) / sqrt(data.nSubj);

        %% Model selection per group
        [quartileModel.alpha{iGroup, iPhase}, quartileModel.exp_r{iGroup, iPhase}, quartileModel.xp{iGroup, iPhase}]                = spm_BMS(bayesianModelData.L{iPhase}(data.Group(:, 2) == iGroup, :)); 
        [quartileModel.alphaPrior{iGroup, iPhase}, quartileModel.exp_rPrior{iGroup, iPhase}, quartileModel.xpPrior{iGroup, iPhase}] = spm_BMS(bayesianModelData.LPrior{iPhase}(data.Group(:, 2) == iGroup, :)); 
    end

    %% Average cue weight global
    groupModel.optW10Mean(iPhase,:)      = mean(bayesianModelData.optW10{iPhase});
    groupModel.optW10SEM(iPhase,:)       = std(bayesianModelData.optW10{iPhase})/sqrt(data.nSubj);
    groupModel.optW10MeanPrior(iPhase,:) = mean(bayesianModelData.optW10Prior{iPhase});
    groupModel.optW10SEMPrior(iPhase,:)  = std(bayesianModelData.optW10Prior{iPhase})/sqrt(data.nSubj);
    
    %% Model selection global
    [groupModel.alpha(iPhase,:),groupModel.exp_r(iPhase,:),groupModel.xp(iPhase,:)]                = spm_BMS(bayesianModelData.L{iPhase}); % all subjects, all 16 models 
    [groupModel.alphaPrior(iPhase,:),groupModel.exp_rPrior(iPhase,:),groupModel.xpPrior(iPhase,:)] = spm_BMS(bayesianModelData.LPrior{iPhase}); % all subjects, all 16 models 
    
end

%% Define databases for winning models
winModel      = [data.Group, bayesianModelData.winModel(:, :).'];
winModelPrior = [data.Group, bayesianModelData.winModelPrior(:, :).'];

winModelQuartile      = [quartileModel.xp{1, 2}; quartileModel.xp{2, 2}; quartileModel.xp{3, 2}; quartileModel.xp{4, 2}].';
winModelQuartilePrior = [quartileModel.xpPrior{1, 2}; quartileModel.xpPrior{2, 2}; quartileModel.xpPrior{3, 2}; quartileModel.xpPrior{4, 2}].';

xlswrite(file.WinModel, winModel)
xlswrite(file.WinModelPrior, winModelPrior)
xlswrite(file.Exceedance, winModelQuartile)
xlswrite(file.ExceedancePrior, winModelQuartilePrior)

%% Plot subject classification in phase 2
figure(1)
histogram(winModel(:,4), 90)
xticks(0:22)
xlim([0, 23])
yticks(0:50)
ylim([0, 20])

figure(2)
histogram(winModelPrior(:,4), 90)
xticks(0:22)
xlim([0, 23])
yticks(0:50)
ylim([0, 20])


%% Go back to start
cd(folder.Scripts)