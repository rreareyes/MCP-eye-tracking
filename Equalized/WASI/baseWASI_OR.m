function baseWASI

%
% Joonkoo Park
% 10/25/2011
%

subid = inputdlg('Subject ID');
subid = str2num(subid{1});


subsess = inputdlg('Run number');
subsess = str2num(subsess{1});

%% Set subject ID and session number, and save data file

workdir = '/home/dantes/Dropbox/Graduate/Lab/Studies/fMRI/WASI';
datadir = [workdir filesep 'data'];
if ~exist(datadir,'dir')
    mkdir(datadir);
end

% if nargin < 2
%     prompt    = {'Enter participant number :','Enter session number :'};
%     dlg_title = 'Participant Information';
%     num_lines = 1;
%     def       = {'0','0'};
%     answer    = inputdlg(prompt,dlg_title,num_lines,def);
%     subid     = str2num(answer{1});
%     subsess   = str2num(answer{2});
% end

% Result file names
res_fn_mat = fullfile(datadir,sprintf('%s-debug-%03g-%02g.mat',mfilename,subid,subsess));
res_fn_txt = fullfile(datadir,sprintf('%s-%03g-%02g.txt',mfilename,subid,subsess));

% Check whether result files already exists for this subject/session
if ~(subid == 0 && subsess == 0)
    while exist(res_fn_mat,'file') || exist(res_fn_txt,'file')
        % Construct a questdlg with three options
        choice = questdlg('File exists. Overwrite?', ...
         'File exists', ...
         'Yes, Overwrite', 'No, Quit', 'No, Quit');
        % Handle response
        switch choice
            case 'Yes, Overwrite'
                overwrite = true;
            case 'No, Quit'
                overwrite = false;
        end
        if overwrite
            break;
        else
            fprintf('Terminating...\n');
            return;
        end
    end
end
clear overwrite;

% Construct data struct
data.subid     = subid;
data.subsess   = subsess;
data.datetime  = datestr(now,31);
data.header    = {'Problem','StimOnset','Cresp','Resp','RT','Accuracy'};

%% Set parameters

params.workdir = workdir;
params.datadir = datadir;

% Screen index (use secondary monitor if it exists)
params.screen.idx = max(Screen('Screens'));

% Key parameters
% Key parameters
if isequal(computer, 'MACI')
    params.button.keyNext   = 'RightArrow';
    params.button.keyBack   = 'LeftArrow';
else
    params.button.keyNext   = 'right';
    params.button.keyBack   = 'left';
end

% Text parameters
params.text.font_instr   = 'Arial';
params.text.size_instr   = 24;
params.text.color_instr  = [255 255 0];
params.text.font_choice  = 'Arial';
params.text.size_choice  = 36;
params.text.color_choice = 255;

% Trial parameters
params.trials.timetotal   = Inf; % 2 minutes

% Write to File
foutid = fopen(res_fn_txt, 'w');
fprintf(foutid, '# Subject : %05g\r\n', data.subid);
fprintf(foutid, '# Session : %02g\r\n', data.subsess);
fprintf(foutid, '# Date    : %s\r\n', data.datetime);
fprintf(foutid, '# Header  : %s %s %s %s %s %s\r\n', data.header{:});

%% PsychToolBox device setup

try
KbName('UnifyKeyNames');

    % Get color indices
    dev.white = WhiteIndex(params.screen.idx);
    dev.black = BlackIndex(params.screen.idx);
    dev.gray  = floor((dev.white + dev.black) / 2);
    dev.red   = [255 0 0];
    dev.green = [0 255 0];
    dev.blue  = [0 0 255];

    % Open window
    dev.win = Screen('OpenWindow', params.screen.idx, dev.black);
    
    % Enable alpha blending with proper blend-function
    Screen('BlendFunction', dev.win, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    Screen('Preference', 'TextRenderer', 1);
    Screen('Preference', 'TextAlphaBlending', 1);
    Screen('Preference', 'TextAntiAliasing', [], 1);
    
    % Disable keyboard input to Matlab
    if isequal(computer, 'PCWIN')
        ListenChar(2);
    end
    
    % Hide cursor
    HideCursor;

    % Get flip interval and set waitframes
    dev.flipint    = Screen('GetFlipInterval', dev.win);
    dev.waitframes = 1;

    % Get screen dimensions (in pixels)
    [params.screen.width params.screen.height] = Screen('WindowSize', dev.win);
    params.screen.centerx = params.screen.width / 2;
    params.screen.centery = params.screen.height / 2;

    % Get keys
    dev.keyNext   = KbName(params.button.keyNext);
    dev.keyBack   = KbName(params.button.keyBack);
    dev.keyReturn = KbName('return');
    dev.cont      = KbName('c');
    if isequal(computer, 'MACI')
        dev.keyesc = KbName('escape');
%         dev.keybackspace = KbName('delete');
    else
        dev.keyesc = KbName('esc');
%         dev.keybackspace = KbName('backspace');
    end
%     dev.keyNum    = KbName({'1!','2@','3#','4$','5%','6^','7&','8*','9(','0)'});
%     dev.keyNumpad = KbName({'1','2','3','4','5','6','7','8','9','0'});
    dev.keyNum    = KbName({'1!','2@','3#','4$','5%'});
    dev.keyNumpad = KbName({'1','2','3','4','5'});
    dev.keyidx    = [dev.keyNum];%, dev.keyNumpad];
    
    % Text position parameters (sx, sy)
    params.position.choice1  = [params.screen.centerx - 340, params.screen.centery + 300];
    params.position.choice2  = [params.screen.centerx - 170, params.screen.centery + 300];
    params.position.choice3  = [params.screen.centerx,       params.screen.centery + 300];
    params.position.choice4  = [params.screen.centerx + 170, params.screen.centery + 300];
    params.position.choice5  = [params.screen.centerx + 340, params.screen.centery + 300];

%% Practice Test

% Instructions
params.instr.expmt{1} = ['Welcome!\n',...
    'You may press the Right Arrow key (next) and the Left Arrow key (back) to navigate through the instruction pages.\n\n',...
    'In this experiment, you will be using buttons 1 through 5 above QWERT keys.\n\n',...
    'Press Right Arrow to continue.'];
params.instr.expmt{2} = ['On each page, I am going to show you some pictures. ',...
    'In each picture, there is a picture missing. Look carefully at all the pieces of each picture and ',...
    'choose the missing piece from the five choices at the bottom of the screen.\n\n',...
    'Press the number that you think is the best answer.'];
params.instr.expmt{3} = ['For your understanding, we will do some practice problems.\n\n',...
    'Press the Enter key to try two simple examples.'];

% Set default font parameters
Screen('TextStyle', dev.win, 0);
Screen('TextFont',  dev.win, params.text.font_instr);
Screen('TextSize',  dev.win, params.text.size_instr);

vbl = GetSecs;
ipage = 1;
while ipage <= length(params.instr.expmt)

    DrawFormattedText(dev.win, params.instr.expmt{ipage}, 'center', 'center', params.text.color_instr, 60, [], [], 1.5);
    vbl = Screen('Flip', dev.win, vbl + dev.flipint);

    keyCode = [];
    if ipage == 1
        dev.keyNav = [dev.keyNext dev.keyesc];
    elseif ipage > 1 && ipage < length(params.instr.expmt)
        dev.keyNav = [dev.keyNext dev.keyBack dev.keyesc];
    elseif ipage == length(params.instr.expmt)
        dev.keyNav = [dev.keyReturn dev.keyBack dev.keyesc];
    end
    while ~any(ismember(dev.keyNav, find(keyCode)))
        [secs keyCode deltaSecs] = KbWait([],3);
    end

    if keyCode(dev.keyBack)
        ipage = ipage - 1;
    elseif keyCode(dev.keyNext)
        ipage = ipage + 1;
    elseif any(keyCode(dev.keyReturn))
        break;
    elseif keyCode(dev.keyesc)
        Screen('CloseAll');
        ShowCursor;
        ListenChar(0);
        Priority(0);
        return;
    end
end


imgmat{1} = imread(fullfile(workdir,'images/MR-A.jpg'));
imgmat{2} = imread(fullfile(workdir,'images/MR-B.jpg'));
corrchoice = [2 5];

Screen('TextFont',  dev.win, params.text.font_choice);
Screen('TextSize',  dev.win, params.text.size_choice);
    
vbl = GetSecs;
dev.startTime = vbl;
numcorr = 0;

for iprac = 1 : length(imgmat)
    
    % Display Problem
    stimonset = vbl + 0.5;
    [responset, resp, dev] = trialMR(dev, params, imgmat{iprac}, stimonset);
    fprintf('Response: %3g in %.3f secs\n', resp, responset - stimonset);
    
    if isequal(resp, corrchoice(iprac))
        % correct
        vbl = displayFeedback(dev, params, 'Correct!');
        numcorr = numcorr + 1;
    else
        % wrong
        vbl = displayFeedback(dev, params, 'Wrong!');
    end
    
    vbl = Screen('Flip', dev.win, vbl + 0.75);
end

%% Math Real Task

% Instructions
params.instr = [];
params.instr.expmt{1} = ['That was the end of the practice problems. ',...
    'If you got any of the two wrong, please let the experimenter know now.\n\n',...
    'Otherwise, press Right Arrow to continue.'];
params.instr.expmt{2} = ['The real experiment will be similar, but there will be more questions.'];
params.instr.expmt{3} = ['If you have any questions, please ask the experimenter. ',...
    'Otherwise, when ready to begin, press Enter to start.'];

% Set default font parameters
Screen('TextStyle', dev.win, 0);
Screen('TextFont',  dev.win, params.text.font_instr);
Screen('TextSize',  dev.win, params.text.size_instr);

vbl = GetSecs;
ipage = 1;
while ipage <= length(params.instr.expmt)

    DrawFormattedText(dev.win, params.instr.expmt{ipage}, 'center', 'center', params.text.color_instr, 60, [], [], 1.5);
    vbl = Screen('Flip', dev.win, vbl + dev.flipint);

    keyCode = [];
    if ipage == 1
        dev.keyNav = [dev.keyNext dev.keyesc];
    elseif ipage > 1 && ipage < length(params.instr.expmt)
        dev.keyNav = [dev.keyNext dev.keyBack dev.keyesc];
    elseif ipage == length(params.instr.expmt)
        dev.keyNav = [dev.keyReturn dev.keyBack dev.keyesc];
    end
    while ~any(ismember(dev.keyNav, find(keyCode)))
        [secs keyCode deltaSecs] = KbWait([],3);
    end

    if keyCode(dev.keyBack)
        ipage = ipage - 1;
    elseif keyCode(dev.keyNext)
        ipage = ipage + 1;
    elseif any(keyCode(dev.keyReturn))
        break;
    elseif keyCode(dev.keyesc)
        Screen('CloseAll');
        ShowCursor;
        ListenChar(0);
        Priority(0);
        return;
    end
end

problemnum = 5 : 35;
corrchoice = [3 1 1 3 5 5 5 2 3 1 2 4 3 1 1 4 5 5 2 2 1 5 4 3 3 4 1 4 2 3 5];
% Set image matrix
% if subsess == 1
%     if mod(subid,2) == 1
%         problemnum = 12 : 2 : 35;
%         corrchoice = [2 1 4 1 4 5 2 5 3 4 4 3];
%     else
%         problemnum = 11 : 2 : 35;
%         corrchoice = [5 3 2 3 1 5 2 1 4 3 1 2 5];
%     end
% elseif subsess == 2
%     if mod(subid,2) == 1
%         problemnum = 11 : 2 : 35;
%         corrchoice = [5 3 2 3 1 5 2 1 4 3 1 2 5];
%     else
%         problemnum = 12 : 2 : 35;
%         corrchoice = [2 1 4 1 4 5 2 5 3 4 4 3];
%     end
% else
%     error('subsess must be either 1 or 2.');
% end
imgmat = cell(length(problemnum),1);
for iimg = 1 : length(problemnum)
    imgmat{iimg} = imread(fullfile(workdir,sprintf('images/MR-%02g.jpg',problemnum(iimg))));
end

Screen('TextFont',  dev.win, params.text.font_choice);
Screen('TextSize',  dev.win, params.text.size_choice);
    
vbl = GetSecs;
dev.startTime = vbl;

for itrial = 1 : length(imgmat)
    
    % Display Problem
    stimonset = vbl + 0.5;
    [responset, resp, dev] = trialMR(dev, params, imgmat{itrial}, stimonset);
    fprintf('Response: %3g in %.3f secs\n', resp, responset - stimonset);
    
    if resp == -1
        fprintf('Time Up\n');
        break;
    else
        if isequal(resp, corrchoice(itrial))
            acc = 1;
        else
            acc = 0;
        end
        fprintf(foutid, '%g\t%5.3f\t%g\t%g\t%5.3f\t%g\r\n', ...
            problemnum(itrial), stimonset - dev.startTime, corrchoice(itrial), resp, responset - stimonset, acc);
        fprintf('Response: %3g in %.3f secs\n', resp, responset - stimonset);
    end
    
    vbl = Screen('Flip', dev.win, responset + 0.2);
end
dev = rmfield(dev,'startTime');

endofexp();

fclose(foutid);

    
catch
    Screen('CloseAll');
    fclose(foutid);
    ShowCursor;
    ListenChar(0);
    Priority(0);
    save(res_fn_mat, 'data', 'params', 'dev');
    assignin('base', 'data', data);         % DEBUG
    assignin('base', 'dev', dev);           % DEBUG
    assignin('base', 'params', params);     % DEBUG
    psychrethrow(psychlasterror);
end % end of try-catch


%% Cleanup

% Close window
Screen('CloseAll');

% Show cursor
ShowCursor;

% Enable keyboard input to Matlab
ListenChar(0);

% Reset priority
Priority(0);

% save data at the workspace
assignin('base', 'data', data);     % DEBUG

%% Nested Functions

    function endofexp()
        
        Screen('TextFont',  dev.win, params.text.font_instr);
        Screen('TextSize',  dev.win, params.text.size_instr);
    
        blockendtext = ['This is the end of this set of trials. Please let the experimenter know.'];

        DrawFormattedText(dev.win, blockendtext, 'center', 'center', params.text.color_instr, 40, [], [], 1.5);
        Screen('Flip', dev.win);
        
        [keyIsDown secs keyCode deltaSecs] = KbCheck;
        while ~any(ismember([dev.cont], find(keyCode)))
            [secs keyCode deltaSecs] = KbWait([],3);
        end
    end

end


%% Sub-functions

function [vbl, val, dev] = trialMR(dev, params, imgmat, onset)
    
    % Question
    tex = Screen('MakeTexture', dev.win, imgmat);
    Screen('DrawTexture', dev.win, tex);
    DrawFormattedText(dev.win, '1', params.position.choice1(1), params.position.choice1(2), dev.white, 400);
    DrawFormattedText(dev.win, '2', params.position.choice2(1), params.position.choice2(2), dev.white, 400);
    DrawFormattedText(dev.win, '3', params.position.choice3(1), params.position.choice3(2), dev.white, 400);
    DrawFormattedText(dev.win, '4', params.position.choice4(1), params.position.choice4(2), dev.white, 400);
    DrawFormattedText(dev.win, '5', params.position.choice5(1), params.position.choice5(2), dev.white, 400);
    Screen('Flip', dev.win, onset);
    
    % Answer input
    val = -1;
    vbl = [];
    [keyIsDown, secs keyCode deltaSecs] = KbCheck;
    while secs < dev.startTime + params.trials.timetotal
        [keyIsDown, secs keyCode deltaSecs] = KbCheck;
        vbl = secs;
        if any(ismember([dev.keyidx dev.keyesc], find(keyCode)))
            % ESC aborts the experiment
            if keyCode(dev.keyesc)
                error('Terminated by user');
            else
                val = KbName(keyCode);
                val = str2double(val(1));
            end
            break;
        end
    end
%     fprintf('Pressed = %g\n', val);
    
end

function vbl = displayFeedback(dev, params, feedbacktext, onset)
    
    % Display Fixation Dot
    DrawFormattedText(dev.win, feedbacktext, 'center', 'center', dev.white, 60, [], [], 1.5);
    if nargin < 4
        vbl = Screen('Flip', dev.win);
    else
        vbl = Screen('Flip', dev.win, onset);
    end

end