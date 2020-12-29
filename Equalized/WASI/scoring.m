datadir = 'C:\Users\rerr_\Google Drive\Graduate\Lab\Studies\MultiCue_Probabilistic\three_phases\WASI\data'; 
cd (datadir);
foo=dir('*.txt');
goo={foo.name};
[u,v] = listdlg('PromptString','Select files:','SelectionMode','multiple','ListString',goo);
n_subs = length(u);
WASI=nan(n_subs,2);
for s = 1:n_subs
    
    filename=goo{u(s)};
    [problem, stimOnset, Cresp, Resp, RT, Accuracy]=textread([datadir '/' filename],'%f/t%f/t%f/t%f/t%f/t%f/t','headerlines',4);
    WASI(s,1)=str2num(filename(10:12));
    WASI(s,2)=sum(Accuracy);
    
end