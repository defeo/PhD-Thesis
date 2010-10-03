\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{these}[2010/09/27 PhD thesis, V1.0]
\PassOptionsToClass{twoside}{memoir}
\PassOptionsToClass{openany}{memoir}
\PassOptionsToClass{a4paper}{memoir}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{memoir}}
\ProcessOptions
\LoadClass{memoir}

%% Maths/CS
\RequirePackage{bbm}
\RequirePackage{amsmath}
\let\equation\gather
\let\endequation\endgather
\RequirePackage{amsthm}
\RequirePackage{amssymb}
\RequirePackage{mathrsfs}
\RequirePackage[noend]{algorithmic}

%% Fonts
\RequirePackage[T1]{fontenc}
\RequirePackage{textcomp}
\renewcommand{\rmdefault}{pplx}
\renewcommand{\sfdefault}{uop}  % Warning: Optima is not in texlive !
\RequirePackage{eulervm}
\RequirePackage[tracking=true]{microtype}

%% Hypertext
\RequirePackage{hyperref}
\hypersetup{pdfborderstyle={/S/U/W 0.5},citebordercolor={0 0.4 1}}
\RequirePackage{bookmark}


%%%%%%%%%%%%%% Style

%% Layout
\settypeblocksize{*}{31pc}{1.75}
\setbinding{1cm}
\setulmargins{*}{*}{1.2}
\setlrmargins{*}{*}{2}
%\setmarginnotes{17pt}{51pt}{\onelineskip}
\setheadfoot{\onelineskip}{2\onelineskip}
\setheaderspaces{*}{2\onelineskip}{*}
\checkandfixthelayout

%% Divisions style
\setsecnumdepth{subsection}
\makechapterstyle{these}{%  needs graphicx package
  \chapterstyle{default}
  \setlength{\beforechapskip}{1.5cm}
  \setlength{\afterchapskip}{1cm}
  \setlength{\midchapskip}{2cm}
  \renewcommand*{\chapnamefont}{\normalfont\normalsize\scshape}
  \renewcommand*{\chapnumfont}{\normalfont\normalsize\bfseries}
  \renewcommand*{\chaptitlefont}{\normalfont\Large\scshape\ifthenelse{\isodd{\thepage}}{\raggedright}{\raggedleft}}
  \renewcommand*{\chapternamenum}{}
  \renewcommand*{\printchaptername}{\ifthenelse{\isodd{\thepage}}{\flushright}{\flushleft}%
    \ifthenelse{\isodd{\thepage}}{\def\side{r}}{\def\side{l}}%
    \makebox[0pt][\side]{\color{gray}{\chapnamefont\@chapapp}}}
  \newcommand*{\chapnumsep}{\hspace{0.5em}}
  \newcommand*{\chapnummargin}{}
  \renewcommand*{\printchapternum}{\ifthenelse{\isodd{\thepage}}%
    {\def\side{l}\def\pleft{\chapnumsep}\def\pright{\chapnummargin}}%
    {\def\side{r}\def\pleft{\chapnummargin}\def\pright{\chapnumsep}}%
    \makebox[0pt][\side]{\resizebox{!}{3ex}{\pleft\chapnumfont\thechapter\pright}}}
  \renewcommand*{\afterchapternum}{\par\hspace{1.5cm}\hrule\vspace{0.2cm}}
  \renewcommand*{\printchapternonum}{\vphantom{\chapnamefont 1}\afterchapternum}
  \renewcommand*{\afterchaptertitle}{\vskip 0.2cm \hrule\vskip\afterchapskip}}
\makeheadstyles{these}{%
  \headstyles{bringhurst}%
  \chapterstyle{these}%
  \setsecheadstyle{\normalfont\memRTLraggedright\scshape}%
  %\setsubsecheadstyle{\sethangfrom{\noindent ####1}\normalfont\itshape\memRTLraggedright}%
  \setsubsubsecheadstyle{\normalfont\normalsize\scshape}%
  %\setparaheadstyle{\normalfont\normalsize\itshape\addperiod}%
  \renewcommand*{\partnamefont}{\normalfont\scshape\huge}
  \renewcommand*{\partnumfont}{\normalfont\scshape\huge}
  \renewcommand*{\parttitlefont}{\normalfont\scshape\Huge}
}
\headstyles{these}

\makeoddhead{Ruled}{}{}{\itshape\rightmark}
\pagestyle{Ruled}

%% Theorems
\newtheoremstyle{mythstyle}{0.5\onelineskip}{0.5\onelineskip}{\itshape}{}{\scshape}{}{1em}{}
\newtheoremstyle{mydefstyle}{0.5\onelineskip}{0.5\onelineskip}{}{}{\scshape}{}{1ex}{}
\newtheoremstyle{myrkstyle}{0.5\onelineskip}{0.5\onelineskip}{}{}{\itshape}{\addperiod}{1em}{}

\theoremstyle{mythstyle}
\newtheorem{theorem}{Theorem}[chapter]
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{corollary}[theorem]{Corollary}
\newtheorem{proposition}[theorem]{Proposition}
\newtheorem{principle}[theorem]{Principle}
\theoremstyle{mydefstyle}
\newtheorem{definition}[theorem]{Definition}
\newtheorem{example}[theorem]{Example}
\theoremstyle{myrkstyle}
\newtheorem{remark}[theorem]{Remark}
\newtheorem*{nota}{Note}

%% Floats
\RequirePackage{float}

% hacking back caption to make it work with titleref
\let\@oldcapt\float@caption
\long\def\float@caption#1[#2]#3{\M@gettitle{#2}\@oldcapt{#1}[#1]{#3}}

% Define two float styles: one with caption on top and one with caption on the bottom
\newcommand\floatc@top[2]{{\@fs@cfont #1:} #2\par}
\newcommand\fs@top{\def\@fs@cfont{\scshape}\let\@fs@capt\floatc@top
  \def\@fs@pre{\hrule height.8pt depth0pt \smallskip}%
  \def\@fs@post{}%
  \def\@fs@mid{\smallskip\hrule\smallskip}%
  \let\@fs@iftopcapt\iftrue%
  \global\let\these@iftop\iftrue}

\newcommand\floatc@bottom[2]{{\@fs@cfont #1:} #2\par}
\newcommand\fs@bottom{\def\@fs@cfont{\scshape}\let\@fs@capt\floatc@bottom
  \def\@fs@pre{}%
  \def\@fs@post{\smallskip\hrule height.8pt depth0pt}%
  \def\@fs@mid{\smallskip\hrule\smallskip}%
  \let\@fs@iftopcapt\iffalse%
  \global\let\these@iftop\iffalse}

% Warning ! This does not work perfectly !
% \these@iftop is redefined when the float is read, rather than when the float
% is typeset. Thus in pages where there's a mixture of top and bottom floats,
% some floats may be inherit it from the next float on the same page.
\newcommand{\topfigrule}{\these@iftop \smallskip\hrule \else \fi}
\newcommand{\botfigrule}{\these@iftop \else \hrule\smallskip \fi}

\floatstyle{bottom}
\restylefloat{figure}
\restylefloat{table}

%% Algorithms
\newcommand*{\algorithmname}{Algorithm}
\newcommand*{\listalgorithmname}{List of Algorithms}
\floatstyle{top}
\newfloat{algorithm}{tbp}{loa}[chapter]
\floatname{algorithm}{\algorithmname}
%\newfloat[chapter]{algorithm}{loa}{\algorithmname}
\newlistof{listofalgorithms}{loa}{\listalgorithmname}
\newlistentry{algorithm}{loa}{0}


\renewcommand{\algorithmicrequire}{\textsc{Input~~~:}}
\renewcommand{\algorithmicensure}{\textsc{Output~:}}
\renewcommand{\algorithmicend}{\textsc{end}}
\renewcommand{\algorithmicif}{\textsc{if}}
\renewcommand{\algorithmicthen}{\textsc{then}}
\renewcommand{\algorithmicelse}{\textsc{else}}
\renewcommand{\algorithmicfor}{\textsc{for}}
\renewcommand{\algorithmicforall}{\textsc{for all}}
\renewcommand{\algorithmicdo}{\textsc{do}}
\renewcommand{\algorithmicwhile}{\textsc{while}}
\renewcommand{\algorithmicloop}{\textsc{loop}}
\renewcommand{\algorithmicrepeat}{\textsc{repeat}}
\renewcommand{\algorithmicuntil}{\textsc{until}}
\renewcommand{\algorithmicprint}{\textsc{print}}
\renewcommand{\algorithmicreturn}{\textsc{return}}
\renewcommand{\algorithmictrue}{\textsc{true}}
\renewcommand{\algorithmicfalse}{\textsc{false}}

%% ToC
\renewcommand*{\cftpartfont}{\scshape\Large}
\renewcommand*{\cftpartpagefont}{}
\renewcommand*{\cftchapterfont}{\scshape}
\renewcommand*{\cftchapterpagefont}{}
\renewcommand*{\cftsectionpagefont}{}
% Add some space before appendices (only if appendix* is used) and the backmatter
\addtodef{\@sapppage}{\addtocontents{toc}{\vspace{\cftbeforepartskip}}}{}
\addtodef{\backmatter}{\addtocontents{toc}{\vspace{\cftbeforepartskip}}}{}

%% Tracking rules for small capitals
\DeclareMicrotypeSet*[tracking]{hugesc}{
  encoding={OT1, T1, T2A, LY1, OT4, QX, T5},
  shape=sc*,
  size={Large-}}
\SetTracking{size={Large}}{50}
\SetTracking[context=romannum]{encoding={OT1, T1, T2A, LY1, OT4, QX, T5}}{0}
\addtoiargdef{\@Roman}{\begingroup\microtypecontext{tracking=romannum}}{\endgroup}

%%%%%%%%%%%%%% Making the class work better with hyperref
%% Modify title and author so that they add the information to the pdf metadata
\addtoiargdef{\title}{}{\hypersetup{pdftitle={#1}}}
\addtoiargdef{\author}{}{\hypersetup{pdfauthor={#1}}}
%% modify appendixpage and backmatter to handle the bookmarks correctly
\addtodef{\@sapppage}{\pdfbookmark[-1]{\appendixpagename}{part:appendices}}{}
\addtodef{\backmatter}{\bookmarksetup{startatroot}}{}


%%%%%%%%%%%%%% Useful commands

%% To move to the third and fourth cover
\newcommand{\thirdcover}{%
  \cleartooddpage\pagestyle{empty}%
  \settypeblocksize{*}{33pc}{1.618}%
  \setbinding{0cm}%
  \setulmargins{*}{*}{1.2}%
  \setlrmargins{*}{*}{1}%
  \checkandfixthelayout%
}
\newcommand{\fourthcover}{\cleartoevenpage\pagestyle{empty}}

%% The list of symbols
\newcommand*{\listsymbname}{List of symbols}
\newcommand*{\listsymbfile}{\jobname.symb}
\newcommand*{\makelistofsymbols}{%
  \makeglossary[\listsymbfile]
  \changeglossnumformat[\listsymbfile]{|hyperpage}}
\newcommand*{\symb}[3][]{%
  \def\@math@protect{\expandafter\strip@prefix\meaning}%
  \def\@tempa{#2}\def\@tempb{#3}%
  \glossary[\listsymbfile](#1){\@math@protect\@tempa}{\@math@protect\@tempb}}
\newcommand{\listofsymbols}[1][]{{
    \ifthenelse{\equal{#1}{}}{\def\@tmpbox{\makebox}\def\@sep{: }}{\def\@tmpbox{\makebox[#1][l]}\def\@sep{}}
    \renewcommand{\glossaryname}{\listsymbname}%
    \renewcommand{\glossarymark}{\markboth{\listsymbname}{\listsymbname}}%
    \renewcommand{\glossitem}[4]{\@tmpbox{##1}\@sep{}##2\cftdotfill{\cftdotsep}##4\\}%
    \printglossary[\listsymbfile]}}
