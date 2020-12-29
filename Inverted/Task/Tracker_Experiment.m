%% Clear the workspace
sca
clear
clc

%% Reseed the random-number generator for each experiment
rng('shuffle');

%% Set base directories and files
folder.Root    = pwd;
folder.Results = fullfile(folder.Root, 'Results');
    
file.Stimulus = 'stim_eyetracker';

if ~exist(folder.Results, 'dir')
    mkdir(folder.Results);
end

%% Set up for debugging
subj.ID  = input('Input subject ID: ','s'); 

[options.Debug, ~] = listdlg('ListString', {'Yes', 'No'}, 'Name', 'Is this a test?', 'ListSize',[250, 300]);

if options.Debug == 1
    %% Set up debug parameters
    file.TrainSequence  = 'debug_sequence'; 
    file.NormalSequence = 'debug_sequence';
    file.ChangeSequence = 'debug_sequence';
    
    options.StartPhase = 3;
    options.StartBlock = 2;
    
    subj.ProbCase = 1;
    options.New = 1;
    
elseif options.Debug == 2
    %% Set up experimental parameters
    file.TrainSequence  = 'exp_sequence'; 
    file.NormalSequence = 'normal_sequence';
    file.ChangeSequence = 'change_sequence';
    
    %% Define starting point
    [options.New, ~] = listdlg('ListString', {'Yes', 'No'}, 'Name', 'New participant?', 'ListSize',[250, 300]); 
        
    % For NEW participants
    if options.New == 1
        options.StartPhase = 1;
        options.StartBlock = 1;
        
        % Ask for probability scenario
        subj.ProbCase = input('Probability assignment case number [1,24]: '); % matches stim.probCase; range = [1 24]

    % For EXISTING participants who had to be restarted
    else
        
        % Define starting phase
        [options.StartPhase, ~] = listdlg('ListString', {'1', '2', '3'}, 'Name', 'Select Phase', 'ListSize', [250, 300]); 

        % Define starting block depending of the phase (1-4 or 1-2)
        if options.StartPhase == 1
            [options.StartBlock, ~] = listdlg('ListString', {'1', '2', '3', '4'}, 'Name', 'Select Block', 'ListSize', [250, 300]); 

        elseif options.StartPhase == 2 || options.StartPhase == 3
            [options.StartBlock, ~] = listdlg('ListString', {'1', '2'}, 'Name', 'Select Block', 'ListSize', [250, 300]); 

        end %End starting point check

    end %End new participant check
    
end %End debug check

%% Timing (in sec)
param.tStim     = 15; % stimulus+response display duration 
param.tFeedback = 0.5; % feedback duration
param.tITI      = 1; % inter-trial-interval 

%% INITIALIZATION
AssertOpenGL;

%% Eye tracker
% Set screen defaults
display.Screen = max(Screen('Screens'));

display.ColorBlack      = 0; % fixation color is "black"
display.ColorBackground = 255; % background color is "white"
display.ColorRed        = [125 0 0];
display.ColorGreen      = [30 125 30];
display.ColorBlue       = [0 0 125];

%Set model to load defaults
settings = SMITE.getDefaults('RED250mobile');

% settings.doAverageEyes = false;
settings.cal.bgColor   = display.ColorBackground/2;

% Custom calibration drawer
calViz                    = AnimatedCalibrationDisplay();
settings.cal.drawFunction = @calViz.doDraw;

%Initialize
tracker = SMITE(settings);
tracker.init();

Screen('Preference', 'SyncTestSettings', 0.002); % the systems are a little noisy, give the test a little more leeway
[window, rect] = PsychImaging('OpenWindow', display.Screen, display.ColorBackground);
hz             = Screen('NominalFrameRate', window);
Priority(1);

Screen('BlendFunction', window, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
Screen('Preference', 'TextAlphaBlending', 1);
Screen('Preference', 'TextAntiAliasing', 2);

% This preference setting selects the high quality text renderer on
% each operating system: It is not really needed, as the high quality
% renderer is the default on all operating systems, so this is more of
% a "better safe than sorry" setting.
Screen('Preference', 'TextRenderer', 1);

%% Stimulus settings
[c(1), c(2)] = RectCenter(rect);

display.ScrSizePix(1) = rect(3); % screen width in pixel
display.ScrSizePix(2) = rect(4); % screen height in pixel

display.pxCueSize         = 600; % stimulus size in pixels; 500x500 square
display.dispLocation(1,:) = [c(1) - display.ScrSizePix(1)/4 - display.pxCueSize/2, c(2)-display.pxCueSize/2, c(1) - display.ScrSizePix(1)/4 + display.pxCueSize/2, c(2) + display.pxCueSize/2]; % left stimulus
display.dispLocation(2,:) = [c(1) + display.ScrSizePix(1)/4 - display.pxCueSize/2, c(2)-display.pxCueSize/2, c(1) + display.ScrSizePix(1)/4 + display.pxCueSize/2, c(2) + display.pxCueSize/2]; % right stimulus

%% Text settings
Screen('TextFont', window, 'Arial');
Screen('TextSize', window, 45);

%% Response
KbName('UnifyKeyNames');  % Enable unified mode of KbName, so KbName accepts identical key names on all operating systems

param.keyRight = KbName('RightArrow'); % right index; right arrow
param.keyLeft  = KbName('LeftArrow');  % left index; left arrow
param.keyGo    = KbName('g');
param.keyStop1 = KbName('s');
param.keyStop2 = KbName('t');

ListenChar(2); % get key responses but suppresses output to the command window
HideCursor;

%% Make textures 
Screen('FillRect', window, display.ColorBackground);
DrawFormattedText(window, 'Loading...', 'center', 'center', display.ColorBackground);
Screen('Flip', window);

%% Fixation point
[Fixationimage, map, alpha] = imread('Fixation.png');
Fixationimage(:, :, 4) = alpha;
tex = Screen('MakeTexture', window, Fixationimage);
Screen('Flip', window);

%% Get stimulus
load(file.Stimulus)
nCues = 16;
nPhases = 3;
    
matStim = cell(nCues);
for iS = 1 : nCues
    matStim{iS} = Screen('MakeTexture', window, stimJPG{iS});
end


%% Loop trhough the probabilistic scenarios
for iPhase = options.StartPhase:nPhases

    %% Get base parameters for stimulus on different phases
    if iPhase == 1
        load(file.TrainSequence, 'stim')
        param.namePhase = {'TP'}; 

    elseif iPhase == 2
        load(file.NormalSequence, 'stim')
        param.namePhase = {'NP'};

    elseif iPhase == 3
        load(file.ChangeSequence, 'stim')
        param.namePhase = {'EP'};

    end

    %% Get base parameters for stimulus on different phases
    if options.New == 1

        %% Set parameter for current phase
        param.TrialSequenceID = ceil(rand(1, 1) * 10); % assign randomized sequence ID
        param.nBlockPhase     = stim.nBlockPhase; 
        param.nTrialBlock     = stim.nTrialBlock; 
        param.nTrialPhase     = stim.nTrialPhase; 
        param.probCase        = stim.probCase(subj.ProbCase, :); % probability assigned to each cue dimension

        %% Create combinations of stimuli
        param.randTrialSequenceBlock{1} = stim.randTrialSequenceBlock{param.TrialSequenceID(1), 1};
        param.probFeedback{1}           = stim.probFeedback{subj.ProbCase}(stim.randTrialSequence{1}(:, param.TrialSequenceID(1)), :); % feedback probability matrix matched to param.seqTrial
        param.probFeedback10{1}         = stim.probFeedback10{subj.ProbCase}(stim.randTrialSequence{1}(:, param.TrialSequenceID(1)), :); % feedback probability matrix matched to param.seqTrial

        for iBlock = 1 : param.nBlockPhase
            param.matCombinationL{1}(:, iBlock) = stim.matCombination(param.randTrialSequenceBlock{1}(:, iBlock), 1); % left stimuli
            param.matCombinationR{1}(:, iBlock) = stim.matCombination(param.randTrialSequenceBlock{1}(:, iBlock), 2); % right stimuli
            param.extFeedbackProb{1}(:, iBlock) = stim.probFeedback{subj.ProbCase}(param.randTrialSequenceBlock{1}(:, iBlock), 6); % feedback probability matrix matched to param.seqTrial % feedback probability extracted

        end

        %% Save experimental parameters
        save(fullfile(folder.Results, [subj.ID '_expParameters_' param.namePhase{1}]), 'param');

    elseif options.New == 2
        load(fullfile(folder.Results, [subj.ID '_expParameters_' param.namePhase{1}]), 'param');
        options.New = 1; %set the check off to continue with the next phase setting up the parameters from the beginning

    end

    %% EXPERIMENT

    %% Perform eye tracker calibration 
    smi.calVal{1} = tracker.calibrate(window, true); % clear recording buffer to make sure any lingering shit from a previous session is removed

    %% Instructions
    %% Text settings
    Screen('TextFont', window, 'Arial');
    Screen('TextSize', window, 45);
    Screen('FillRect', window, display.ColorBackground);

    if iPhase == 1
        DrawFormattedText(window, 'First Round', 'center', c(2)-260, display.ColorGreen); % displayed in green
        DrawFormattedText(window, 'Your goal is to win as many trials as possible', 'center', c(2) - 180, display.ColorBlack);

    elseif iPhase == 2
        DrawFormattedText(window, 'Second Round','center', c(2)-260, display.ColorGreen); 
        DrawFormattedText(window, 'The task is the same as before', 'center', c(2)-180,display.ColorBlack);
        DrawFormattedText(window, 'Your goal is to win as many trials as possible', 'center', c(2) - 100, display.ColorBlack);

    elseif iPhase == 3
        DrawFormattedText(window, 'Third Round','center', c(2)-260, display.ColorGreen); 
        DrawFormattedText(window, 'The task is the same as before','center', c(2)-180, display.ColorBlack);
        DrawFormattedText(window, 'Your goal is to win as many trials as possible', 'center', c(2) - 100, display.ColorBlack);

    end

    Screen('DrawTexture', window, tex);

    DrawFormattedText(window, 'Please don''t remove your hand from the keyboard during the experiment', 'center', c(2) + 100, display.ColorBlue); % displayed in blue
    DrawFormattedText(window, 'Press RIGHT ARROW key to continue', 'center', c(2) + 180, display.ColorBlack);

    Screen(window, 'Flip'); 

    while 1 % wait for subject's response
        [keyIsDown, timeKeyPress, keyCode] = KbCheck;

        if keyCode(param.keyRight)
            break;

        end
    end

    %% BLOCK
    timePhase = GetSecs; % record start time of the phase

    for iBlock = options.StartBlock : param.nBlockPhase

        %% Initialize data matrices
        dE.randNumFeedback = zeros(param.nTrialBlock, 1); % random number drawn in each trial
        dE.keyResp         = NaN(param.nTrialBlock, 1); % record left(1)/right(-1) key response
        dE.RT              = NaN(param.nTrialBlock, 1); % record RT
        dE.winTrial        = zeros(param.nTrialBlock, 1); % record actual feedback given
        dE.timeTrial       = zeros(param.nTrialBlock, 1); % records duration of each trial
        dE.timeStimCum     = zeros(param.nTrialBlock, 1); % records cumulative time from the phase starting point
        dE.timeFbCum       = zeros(param.nTrialBlock, 1);
        dE.points          = zeros(param.nTrialBlock, 1); % records amount of points obtained
        dE.bestDecision    = zeros(param.nTrialBlock, 1); % records optimal choice trials
        dE.leftWeight      = zeros(param.nTrialBlock, 1); % records left cue weight
        dE.rightWeight     = zeros(param.nTrialBlock, 1); % records right cue weight

        %% Block Introduction
        if iBlock > 1 % skip the first block intro (there's a phase intro instead)
            initPhase = ['Block ' num2str(iBlock) '/' num2str(param.nBlockPhase)];
            DrawFormattedText(window, initPhase, 'center', c(2) - 260, display.ColorBlack);
            DrawFormattedText(window, 'Your goal is to win as many trials as possible.', 'center', c(2) - 180, display.ColorBlack);
            DrawFormattedText(window, '+', 'center', 'center', display.ColorBlack);
            DrawFormattedText(window, 'Please don''t remove your hand from the keyboard during the experiment', 'center', c(2) + 100, display.ColorBlue); % displayed in blue
            DrawFormattedText(window, 'Press RIGHT ARROW key to continue', 'center', c(2) + 180, display.ColorBlack);
            Screen(window, 'Flip');

            while 1 % wait for subject's response
                [keyIsDown, timeKeyPress, keyCode] = KbCheck;

                if keyCode(param.keyRight)
                    break;

                end %End check for key press

            end %End wait for key press

        end %End block check

        %% Countdown 5 sec -> 1 sec fixation
        for tC = 5 : -1 : 0
            Screen('FillRect', window, display.ColorBackground);

            if tC > 0
                DrawFormattedText(window, num2str(tC), 'center', 'center', display.ColorBlack);

            else

                Screen('DrawTexture', window, tex);

            end 

            Screen(window, 'Flip');
            WaitSecs(1);

        end

        %% START TRIAL
        for iTrial = 1 : param.nTrialBlock
            timeTrial = GetSecs;
            FlushEvents('keyDown'); flagKeypress = 0; % reset key press
            dE.randNumFeedback(iTrial) = rand(1); % draw a random number (used for feedback determination)

            %% Start collecting data from the tracker 
            tracker.startRecording();

            if iTrial == 1
                tracker.startBuffer();

            end

            %% Periodically check if the tracker is still working
            tracker.processError(tracker.isConnected(), 'No longer connected to eye tracker');

            %% Stimuli presentation
            stimulus.Name = ['L_' num2str(param.matCombinationL{1}(iTrial,iBlock)) '-R_' num2str(param.matCombinationR{1}(iTrial, iBlock)) '.jpg'];

            Screen('FillRect', window, display.ColorBackground);
            Screen('DrawTexture', window, tex);
            Screen('DrawTexture', window, matStim{param.matCombinationL{1}(iTrial, iBlock)}, [], display.dispLocation(1, :));
            Screen('DrawTexture', window, matStim{param.matCombinationR{1}(iTrial, iBlock)}, [], display.dispLocation(2, :));

            [~, timeStim, ~, ~, ~] = Screen(window, 'Flip');

            %% Send message to mark trial and start
            tracker.setBegazeTrialImage(stimulus.Name)
            tracker.sendMessage([param.namePhase{1} '_block_' num2str(iBlock) '_trial_' num2str(iTrial)]);

            % Get image of the stimuli presented and save it for BeGaze
            % (only needed once on each device)
            %imageArray = Screen('GetImage', window);
            %imwrite(imageArray, fullfile(folder.Results, stimulus.Name))

            %% Check for button press within stimuli presentation
            while (GetSecs - timeStim) < param.tStim
                if flagKeypress == 0 % record only the first response
                    [keyIsDown, timeKeyPress, keyCode] = KbCheck;

                    if keyIsDown && keyCode(param.keyLeft) > 0 % left key is pressed
                        dE.keyResp(iTrial) = 1; % left = 1
                        dE.RT(iTrial)      = timeKeyPress - timeStim;

                        %% Tracker: write which button was pressed
                        tracker.setBegazeKeyPress('LeftArrow')

                        if dE.randNumFeedback(iTrial) < param.extFeedbackProb{1}(iTrial, iBlock) % win
                            dE.winTrial(iTrial) = 1;
                        end

                        flagKeypress = 1;

                    elseif keyIsDown && keyCode(param.keyRight) > 0 % right key is pressed
                        dE.keyResp(iTrial) = -1; % right = -1
                        dE.RT(iTrial)      = timeKeyPress - timeStim;

                        %% Tracker: write which button was pressed
                        tracker.setBegazeKeyPress('RightArrow')

                        if dE.randNumFeedback(iTrial) >= param.extFeedbackProb{1}(iTrial, iBlock) % win
                            dE.winTrial(iTrial) = 1;

                        end

                        flagKeypress = 1;

                    end
                end

                if flagKeypress == 1 % if a response is made, move on
                    break;

                end

            end

            %% Stop recording once we get a response
            tracker.stopRecording();

            %% Set adaptive threshold for RT
            if iTrial < 5
                dE.thresholdSlow(iTrial) = 13;
                dE.thresholdFast(iTrial) = 4;
            else
                dE.thresholdSlow(iTrial) = nanmean(dE.RT) + nanstd(dE.RT);
                dE.thresholdFast(iTrial) = nanmean(dE.RT) - (0.5 * nanstd(dE.RT));
            end

            %% Probabilistic feedback
            % This compares a random number between 0-1 vs the weight of
            % the selected cue. If larger is a win. 
            Screen('FillRect', window, display.ColorBackground);
            Screen('TextSize', window, 60);

            if isnan(dE.keyResp(iTrial))  % no response
                DrawFormattedText(window, 'Miss', 'center', 'center', display.ColorRed);
                DrawFormattedText(window, '0', 'center', c(2)+100, display.ColorRed);
                dE.points(iTrial) = 0; 

            elseif dE.winTrial(iTrial) == 1 && dE.RT(iTrial) <= dE.thresholdSlow(iTrial) && dE.RT(iTrial) >= dE.thresholdFast(iTrial) %% mean+std > RT > mean-std (Normal range)
                DrawFormattedText(window, 'Win','center', 'center', display.ColorGreen);
                DrawFormattedText(window, '+1','center', c(2) + 100, display.ColorGreen);
                dE.points(iTrial) = 1; 

            elseif dE.winTrial(iTrial) == 1 && dE.RT(iTrial) < dE.thresholdFast(iTrial) %% RT < mean-std (Fast range)
                DrawFormattedText(window, 'Win', 'center', 'center', display.ColorGreen);
                DrawFormattedText(window, '+2', 'center', c(2) + 100, display.ColorGreen);
                dE.points(iTrial) = 2;            

            elseif dE.winTrial(iTrial) == 1 && dE.RT(iTrial) > dE.thresholdSlow(iTrial) %% RT > mean + std (Slow threshold)
                DrawFormattedText(window, 'Win but SLOW', 'center', 'center', display.ColorRed);
                DrawFormattedText(window, '0','center', c(2) + 100, display.ColorBlack);
                  dE.points(iTrial) = 0; 

            elseif dE.winTrial(iTrial) == 0 %% Lost trial
                DrawFormattedText(window, 'Lose', 'center', 'center', display.ColorRed);
                DrawFormattedText(window, '0', 'center', c(2) + 100, display.ColorRed);
                dE.points(iTrial) = 0; 

            end

            [timeVBL, timeFb, timeFlip, Missed, Beampos] = Screen(window, 'Flip');

            while (GetSecs - timeFb) < param.tFeedback
            end

            %% Check if participant's choice was the best
            % All is relative to the left stimuli. Since max probability = 1.0
            % L = 1-Right & R = 1-Left
            % L > 0.5 ---> L wins
            % L < 0.5 ---> R wins
            if param.extFeedbackProb{1}(iTrial, iBlock) > 0.5 && dE.keyResp(iTrial) == 1 
                dE.bestDecision(iTrial) = 1;

            elseif param.extFeedbackProb{1}(iTrial, iBlock) > 0.5 && dE.keyResp(iTrial) == -1
                dE.bestDecision(iTrial) = 0;

            elseif param.extFeedbackProb{1}(iTrial, iBlock) < 0.5 && dE.keyResp(iTrial) == -1
                dE.bestDecision(iTrial) = 1;

            elseif param.extFeedbackProb{1}(iTrial, iBlock) < 0.5 && dE.keyResp(iTrial) == 1
                dE.bestDecision(iTrial) = 0;

            elseif param.matCombinationL{1}(iTrial, iBlock) == 0.5 % if left and right weights are the same, discard trial
                dE.bestDecision(iTrial) = NaN;
            end

            %% Get the weight of each cue
            dE.leftWeight(iTrial)  = param.extFeedbackProb{1}(iTrial, iBlock); 
            dE.rightWeight(iTrial) = 1 - dE.leftWeight(iTrial); 

            %% ITI
            Screen('FillRect', window, display.ColorBackground);
            Screen('TextSize', window, 45);
            Screen('DrawTexture', window, tex);
            Screen(window, 'Flip');
            WaitSecs(param.tITI);

            dE.timeTrial(iTrial)   = GetSecs - timeTrial; % records duration of each trial
            dE.timeStimCum(iTrial) = timeStim - timePhase; % records cumulative stim onset since the phase start time
            dE.timeFbCum(iTrial)   = timeFb - timePhase; % records cumulative feedback onset since the phase start time

        end %End of trial loop

        %% Performance summary (diplayed at the end of each block)
        dE.cumPoints(1) = nansum(dE.points); % number of win trials
        dE.cumPoints(2) = nansum(dE.winTrial) / param.nTrialBlock * 100; % percentage of win trials
        textScorePhase  = ['Points earned in this block: ' num2str(dE.cumPoints(1)) ' (' num2str(round(dE.cumPoints(2))) '% win)'];

        Screen('FillRect',window, display.ColorBackground);
        DrawFormattedText(window, ['This is the end of Block ' num2str(iBlock)], 'center',c(2) - 150, display.ColorBlack);
        DrawFormattedText(window, textScorePhase, 'center', 'center', [0, 153, 0]); % displayed in green
        DrawFormattedText(window, 'Please take a short break.', 'center', c(2) + 150, display.ColorBlack);
        Screen(window, 'Flip');         

        %% SAVE DATA AFTER EACH BLOCK
        % Tracker
        data = tracker.consumeBufferData(); 
        tracker.stopBuffer();
        WaitSecs(0.5);
        tracker.saveData(fullfile(folder.Results, [subj.ID '_' param.namePhase{1} '_Block' num2str(iBlock)]), subj.ID, 'test', true);

        % Responses
        save(fullfile(folder.Results, [subj.ID '_' param.namePhase{1} '_Block' num2str(iBlock)]), 'dE', 'iBlock'); 

        WaitSecs(3);

    end %End block loop

    %% Ending text
    Screen('FillRect', window, display.ColorBackground);
    DrawFormattedText(window, 'This is the end of the Phase', 'center', 'center', display.ColorBlack);
    DrawFormattedText(window, 'Please wait for the experimenter.', 'center', c(2) + 150, display.ColorBlack);
    Screen(window, 'Flip');

    while 1
        [keyIsDown, timeKeyPress, keyCode] = KbCheck;

        if keyIsDown && keyCode(param.keyGo) > 0
            break;

        end
    end

end %End phase loop


tracker.deInit(true);
        
%% Calculate points
finalResult = [];
phaseNames = {'TP', 'NP', 'EP'};

if options.Debug == 1
    finalResult = 50;
    
elseif options.Debug == 2
    
    for iPhase = 1:nPhases
        nBlocks = [4, 2, 2];

        for iBlock = 1:nBlocks(iPhase)
            load(fullfile(folder.Results, [subj.ID '_' phaseNames{iPhase} '_Block' num2str(iBlock)]))
            finalResult = vertcat(finalResult, dE.cumPoints(1));
            finalResult = sum(finalResult);

        end
    end
end

%% Show Results
Screen('FillRect', window, display.ColorBackground);
DrawFormattedText(window, 'Please wait, calculating your results...', 'center', 'center', display.ColorBlack);
Screen(window, 'Flip');

WaitSecs(2);

Screen('FillRect', window, display.ColorBackground);
Screen('TextSize', window, 60);
DrawFormattedText(window, ['Congratulations! You won ' num2str(finalResult) ' points'], 'center', 'center', display.ColorBlue);
Screen(window, 'Flip');

while 1
    [keyIsDown, timeKeyPress, keyCode] = KbCheck;
    
    if keyIsDown && keyCode(param.keyGo) > 0
        break;

    end
    
end

%% EXIT EXPERIMENT
Screen('CloseAll');
ListenChar(0);
ShowCursor;


disp(['Congratulations! You won ' num2str(finalResult) ' points'])
