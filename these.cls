\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{these}[2010/09/27 PhD thesis, V1.0]
\LoadClass[a4paper,twoside,openany,oldfontcommands]{memoir}

%% Maths/CS
\RequirePackage{bbm}
\RequirePackage{amsmath}
\let\equation\gather
\let\endequation\endgather
\RequirePackage{amsthm}
\RequirePackage{amssymb}
\RequirePackage{mathrsfs}
\RequirePackage[noend]{algorithmic}
\RequirePackage[final]{listings}

%% Fonts
\RequirePackage[T1]{fontenc}
\RequirePackage{textcomp}
\RequirePackage{tgheros}
\renewcommand{\rmdefault}{pplx}
\RequirePackage{eulervm}

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
    \makebox[0pt][\side]{\color{gray}{\chapnamefont Chapter}}}
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

%% Algorithms
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

\newcommand*{\algorithmname}{Algorithm}
\newcommand*{\listalgorithmname}{List of Algorithms}
\newlistof{listofalgorithms}{loa}{\listalgorithname}
\newfloat[chapter]{algorithm}{loa}{\algorithmname}

%% Floats
\captionnamefont{\normalfont\scshape}


%% Code listings
\lstset{
  upquote=true,
  basicstyle=\ttfamily,          % print whole listing in typewriter
  keywordstyle=\color{blue}, % bold blue keywords
  %identifierstyle=,           % nothing happens
  commentstyle=\color{green}, % green comments
  stringstyle=\color{red},      % typewriter type for strings
  showstringspaces=false     % no special string spaces
}

%% ToC
\newcommand{\leader}{\hspace{2em}}
\renewcommand*{\cftpartfont}{\scshape\Large}
\renewcommand*{\cftpartpagefont}{}
\renewcommand*{\cftchapterfont}{\scshape}
\renewcommand*{\cftchapterpagefont}{}
\renewcommand*{\cftsectionpagefont}{}
%\settowidth{\cftpartnumwidth}{\cftpartfont III~}
