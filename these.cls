%% these.cls
%% Copyright 2010 Luca De Feo
% 
% To the extent possible under law, Luca De Feo has waived all
% copyright and related or neighboring rights to these.cls
%
% http://creativecommons.org/publicdomain/zero/1.0/


\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{these}[2010/09/27 PhD thesis, V1.0]

\newif\ifartwork
\newif\ifbw
\artworktrue
\bwfalse
\DeclareOption{a4layout}{\PassOptionsToClass{a4paper}{memoir}\def\@layout{a4}}
\DeclareOption{a4papersaver}{\PassOptionsToClass{a4paper}{memoir}\def\@layout{a4ps}}
\DeclareOption{a5layout}{\PassOptionsToClass{a5paper}{memoir}\def\@layout{a5}}
\DeclareOption{b5layout}{\PassOptionsToClass{b5paper}{memoir}\def\@layout{b5}}
\DeclareOption{noartwork}{\artworkfalse}
\DeclareOption{artwork}{\artworktrue}
\DeclareOption{bw}{\bwtrue}
\DeclareOption{color}{\bwfalse}
\PassOptionsToClass{twoside}{memoir}
\PassOptionsToClass{openany}{memoir}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{memoir}}
\ProcessOptions*
\LoadClass{memoir}

%% Maths/CS
\RequirePackage{bbm}
\RequirePackage{amsmath}
\RequirePackage{amsthm}
\RequirePackage{amssymb}
\RequirePackage{mathrsfs}
\RequirePackage[noend]{algorithmic}
\let\equation\gather
\let\endequation\endgather
\displaywidowpenalty=150
\postdisplaypenalty=10

%% Fonts
\RequirePackage[T1]{fontenc}
\RequirePackage{textcomp}
\renewcommand{\rmdefault}{pplx}
\renewcommand{\sfdefault}{uop}  % Warning: Optima is not in texlive !
\RequirePackage[T1]{eulervm}
\RequirePackage[tracking=true]{microtype}
\RequirePackage{xcolor}

%% Others
\RequirePackage{float}
\ifartwork \RequirePackage{graphicx} \fi
\RequirePackage{xstring}

%% Hypertext
\RequirePackage{hyperref}
\hypersetup{pdfborderstyle={/S/U/W 0.5},citebordercolor={0 0.4 1}}
\RequirePackage{bookmark}

%%%%%%%%%%%%%% Style

%% Layout
\newif\ifafour
\newif\ifafive
\newif\ifbfive
\newif\ifafourps

\def\@temp{a4}
\ifx \@temp \@layout
  \afourtrue
  \settypeblocksize{*}{31pc}{1.75}
  \setulmargins{*}{*}{1.2}
  \setlrmargins{*}{*}{1.7}
  % \setmarginnotes{17pt}{51pt}{\onelineskip}
  \setheadfoot{\onelineskip}{2\onelineskip}
  \setheaderspaces{*}{2\onelineskip}{*}
  \checkandfixthelayout

  \def\back@cover@layout{
    \setlrmargins{*}{*}{1}%
    \checkandfixthelayout%
  }
\else \fi
\def\@temp{a4ps}
\ifx \@temp \@layout
  \afourpstrue
  \settypeblocksize{714pt}{448.13pt}{*}
  \setulmargins{*}{*}{1.2}
  \setlrmargins{*}{*}{1.5}
  % \setmarginnotes{17pt}{51pt}{\onelineskip}
  \setheadfoot{\onelineskip}{2\onelineskip}
  \setheaderspaces{*}{2\onelineskip}{*}
  \checkandfixthelayout

  \def\back@cover@layout{
    \setlrmargins{*}{*}{1}%
    \checkandfixthelayout%
  }
\else \fi
\def\@temp{a5}
\ifx \@temp \@layout
  \afivetrue
  \settypeblocksize{*}{27pc}{1.5}
  \setulmargins{*}{*}{1.2}
  \setlrmargins{*}{*}{2}
  % \setmarginnotes{17pt}{51pt}{\onelineskip}
  \setheadfoot{\onelineskip}{2\onelineskip}
  \setheaderspaces{*}{2\onelineskip}{*}
  \checkandfixthelayout

  \def\back@cover@layout{
    \setlrmargins{*}{*}{1}%
    \checkandfixthelayout%
  }
\else \fi
\def\@temp{b5}
\ifx \@temp \@layout
  \bfivetrue
  \settypeblocksize{210mm}{30pc}{*}
  \setulmargins{*}{*}{1.2}
  \setlrmargins{*}{*}{1.7}
  % \setmarginnotes{17pt}{51pt}{\onelineskip}
  \setheadfoot{\onelineskip}{2\onelineskip}
  \setheaderspaces{*}{2\onelineskip}{*}
  \checkandfixthelayout

  \def\back@cover@layout{
    \setlrmargins{*}{*}{1}%
    \checkandfixthelayout%
  }
\else \fi

%% Colors
\ifbw
  \definecolor{chaptercolor}{gray}{0.5}
\else
  \definecolor{chaptercolor}{rgb}{0.83,0.3,0.15}
\fi


%% Divisions style
\setsecnumdepth{subsection}
\makechapterstyle{these}{%  needs graphicx package
  \chapterstyle{default}
  \setlength{\beforechapskip}{1.5cm}
  \setlength{\afterchapskip}{1cm}
  \setlength{\midchapskip}{2cm}
  \renewcommand*{\chapnamefont}{\normalfont\normalsize\scshape\color{chaptercolor}}
  \renewcommand*{\chapnumfont}{\normalfont\normalsize\bfseries\color{chaptercolor}}
  \renewcommand*{\chaptitlefont}{\normalfont\Large\scshape\ifthenelse{\isodd{\thepage}}{\raggedright}{\raggedleft}}
  \renewcommand*{\chapternamenum}{}
  \renewcommand*{\printchaptername}{\ifthenelse{\isodd{\thepage}}{\flushright}{\flushleft}%
    \ifthenelse{\isodd{\thepage}}{\def\side{r}}{\def\side{l}}%
    \makebox[0pt][\side]{\chapnamefont\@chapapp}}
  \newcommand*{\chapnumsep}{\hspace{0.5em}}
  \newcommand*{\chapnummargin}{}
  \renewcommand*{\printchapternum}{\ifthenelse{\isodd{\thepage}}%
    {\def\side{l}\def\pleft{\chapnumsep}\def\pright{\chapnummargin}}%
    {\def\side{r}\def\pleft{\chapnummargin}\def\pright{\chapnumsep}}%
    \makebox[0pt][\side]{\resizebox{!}{3ex}{\pleft\chapnumfont\thechapter\pright}}}
  \renewcommand*{\afterchapternum}{\par\vspace{\onelineskip}%
    \ifthenelse{\isodd{\thepage}}{\def\side{l}\potiron\hfill}{}
    \begin{minipage}{\titleboxwidth}\hrule\vspace{0.2cm}}
  \renewcommand*{\printchapternonum}{\vphantom{\chapnamefont 1}\afterchapternum}
  \renewcommand*{\afterchaptertitle}{\end{minipage}%
    \ifthenelse{\isodd{\thepage}}{}{\hfill\def\side{r}\potiron}%
    \vskip 0.2cm \hrule\vskip\afterchapskip}}
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


%% Artwork
% The directory containing artworks
\def\artdir{artwork}
\newlength{\titleboxwidth}

\ifartwork
  \def\bw@suffix{\ifbw\else-color\fi}

  % The chapter pumpkin
  \def\potiron{\raisebox{-0.8\onelineskip}[0pt][0pt]{%
      \def\tmp@side{l}
      \if\side\tmp@side\def\tmp@reflection{\reflectbox}\else\def\tmp@reflection{}\fi
      \makebox[0pt][\side]{\tmp@reflection{\includegraphics[width=0.25\textwidth]{\artdir/potiron\bw@suffix}}}}}
  \setlength{\titleboxwidth}{0.77\textwidth}

  % Halmos' snail
  \renewcommand{\qedsymbol}{\raisebox{-0.2ex}{\includegraphics[height=2ex]{\artdir/escargot}}}

  % Part illustration
  \renewcommand{\printparttitle}[1]{\parttitlefont #1%
    \StrSubstitute{#1}{ }{_}[\@filename]%
    \def\p@th{\artdir/\@filename\bw@suffix}%
    \IfGraphicsExists{\p@th}{%
      \par\vfil%
      \includegraphics[width=\textwidth,height=0.5\textheight,keepaspectratio=true]{\p@th}%
    }{\ClassWarning{these}{Artwork file \p@th\space not found. Skipping it.}}}
\else
  \def\potiron{}
  \setlength{\titleboxwidth}{\textwidth}
\fi


%% Theorems
% Hack for titleref
\let\old@thm\@begintheorem
\def\@begintheorem#1#2[#3]{\@ifempty{#3}{\M@gettitle{#1~#2}}{\M@gettitle{#3}}\old@thm{#1}{#2}[{#3}]}

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
% hacking back caption to make it work with titleref
\let\@oldcapt\float@caption
\long\def\float@caption#1[#2]#3{\M@gettitle{#2}\@oldcapt{#1}[{#2}]{#3}}

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
\renewcommand{\algorithmicto}{\textsc{to}}
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
\SetTracking{encoding={OT1, T1, T2A, LY1, OT4, QX, T5},size={Large}}{50}
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
  \ifx \back@cover@layout \undefined%
  \else \back@cover@layout \fi%
}
\newcommand{\fourthcover}{\cleartoevenpage\pagestyle{empty}}

%% The list of symbols
\newcommand*{\listsymbname}{List of symbols}
\newcommand*{\listsymbfile}{\jobname.symb}
\newcommand*{\makelistofsymbols}{%
  \makeglossary[\listsymbfile]
  \changeglossnumformat[\listsymbfile]{|hyperpage}}
\newcommand*{\symb}[3][]{%
  \ifshowindexmark\@showidx{#2}\fi
  \def\@math@protect{\expandafter\strip@prefix\meaning}%
  \def\@tempa{#2}\def\@tempb{#3}%
  \glossary[\listsymbfile](#1){\@math@protect\@tempa}{\@math@protect\@tempb}}
\newcommand{\listofsymbols}[1][]{{
    \ifthenelse{\equal{#1}{}}{\def\@tmpbox{\makebox}\def\@sep{: }}{\def\@tmpbox{\makebox[{#1}][l]}\def\@sep{}}
    \renewcommand{\glossaryname}{\listsymbname}%
    \renewcommand{\glossarymark}{\markboth{\listsymbname}{\listsymbname}}%
    \renewcommand{\glossitem}[4]{\@tmpbox{##1}\@sep{}##2\cftdotfill{\cftdotsep}##4\\}%
    \printglossary[\listsymbfile]}}

%% Conditionally executes #2 or #3, depending on whether #1 is an existing
%% filename (with or without extension) for the graphicx pacakge
\newcommand{\IfGraphicsExists}[3]{
  \newif\if@ext%
  \IfFileExists{#1}{\@exttrue}{}%
  \@for\@ext:=\Gin@extensions\do{%
    \if@ext\else\IfFileExists{#1\@ext}{\@exttrue}{}\fi}%
  \if@ext #2 \else #3 \fi}