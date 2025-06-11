%% Subclass of infthesis with my personal additions.
%% This is useful with multiple files to contain common preamble parts.
%%
%% Author: Filip Smola

\NeedsTeXFormat{LaTeX2e}[1994/12/01]

%% --- Options ---

\RequirePackage{ifthen}

%% Set up feedback tools, with option to turn off
\newboolean{feedback}
\DeclareOption{feedback}{\setboolean{feedback}{true}}
\DeclareOption{nofeedback}{\setboolean{feedback}{false}}

%% Set default options and process the ones given
\ExecuteOptions{feedback}
\ProcessOptions

%% --- Parent Class ---

%% Load parent class
\LoadClass[phd,aiai,abbrevs]{infthesis}

%% --- Main Code ---

%% Select UK English for language-specific features
\RequirePackage[UKenglish]{babel}

%% Require AMS packages for typesetting maths and theorems
\RequirePackage{amsmath,amssymb,amsfonts,amsthm}

%% Require colour package, mainly for Isabelle highlighting
\RequirePackage{xcolor}

%% Require bussproofs for proof trees
\RequirePackage{bussproofs}

%% Require SVG for figures, with relsize to scale any text in them
\RequirePackage{svg}
\RequirePackage{relsize}

%% Require enumitem for more flexible enumerated list presentation, asking for inline list definitions
\RequirePackage[inline]{enumitem}

%% Require Isabelle code typesetting files
\RequirePackage{isastyle/isabelle}
\RequirePackage{isastyle/isabellesym}
\RequirePackage{isastyle/isabellecolor}
\RequirePackage{isastyle/isabelleman}
\isabellestyle{it}

%% Set Isabelle blocks to use single line spacing
\renewenvironment{isabellebody}{%
\isamarkuptrue\par%
\isa@parindent\parindent\parindent0pt%
\isa@parskip\parskip\parskip0pt%
\isaspacing\isastyle%
\singlespace}{\par}

%% Define further abbreviations for typesetting Isabelle code
\newcommand{\isalist}[1]{{\isacharbrackleft}{#1}{\isacharbrackright}}
\newcommand{\isapars}[1]{{\isacharparenleft}{#1}{\isacharparenright}}
\newcommand{\isabraces}[1]{{\isacharbraceleft}{#1}{\isacharbraceright}}
\newcommand{\isarestran}[1]{{\isasymlparr}{#1}{\isasymrparr}}

%% Define commands for stretching text (aimed at aligning Isabelle code)
\newcommand{\widthtoR}[2]{\rlap{#1}\hphantom{#2}} % Add space to right
\newcommand{\widthtoL}[2]{\hphantom{#2}\llap{#1}} % Add space to left

%% Set up todonotes and chagebars to show if feedback is true (making sure the commands compile in either case)
\RequirePackage{todonotes,changebar}
\ifthenelse{\boolean{feedback}}
  {}
  { \PassOptionsToPackage{disable}{todonotes}
    \nochangebars
  }
\newcommand{\cbar}[1]{{\cbstart}{#1}{\cbend}}

% Small inline tag to note missing citation
\RequirePackage{currfile} % For printing the correct filename in missing citation warning
\newcommand{\tocite}{\todo[color=yellow,noinlinepar,inlinewidth=0.8cm,size=\tiny,inline]{cite}\PackageWarning{smolathesis}{Citation to add in '\currfilename'}}
\newcommand{\toxref}{\todo[color=blue!20,noinlinepar,inlinewidth=0.8cm,size=\tiny,inline]{XRef}\PackageWarning{smolathesis}{Cross-reference to add in '\currfilename'}}

% Small inline tag to note missing content
\newcommand{\todopoint}{\todo[noinlinepar,inlinewidth=0.8cm,size=\tiny,inline]{ToDo}}

% Grey inline box for clarifying notes
\newcommand{\tonote}[1]{\todo[color=lightgray,caption={},inline]{{#1}}} % Caption needed to allow lists in content

% Shorthand for inline box with list of things to do/write/touch on
\newcommand{\tolist}[1]{\todo[caption={},inline]{To do:\begin{itemize}[nosep]#1\end{itemize}}}

% Category of todonotes to highlight supervisor feedback
\newcommand{\stodo}[2][]{\todo[color=blue!20,author=JF,#1]{#2}}

%% Move todonotes to left side, which has wider margin (assumes oneside layout)
\reversemarginpar

%% Set marginpar width to 0.5 cm less than margin set in infthesis, used by todonotes
\setlength{\marginparwidth}{3.5cm}

%% Redefine marginpar to be marginnote, fixing issue (\end in internal vertical mode) with todonotes in standalone
%% Note that this preserves the width we set to marginpar
\RequirePackage{marginnote}
\let\marginpar\marginnote

%% Set up hypertext links
\RequirePackage{hyperref}

%% Require ulem for strikethrough, keeping emphasis normal
\RequirePackage[normalem]{ulem}

%% Subcaptions for complex figures
\RequirePackage{subcaption}

%% Listings for code and program output
\RequirePackage{listings}

%% Quotes package
\RequirePackage{csquotes}

%% For line numbering
\RequirePackage{lineno}

%% For float barrier
\RequirePackage{placeins}

%% For landscape figures
\RequirePackage{rotating}

%% Define the lay summary command based on infthesis abstract
\newenvironment{laysummary}
   {\begin{frontenv}{Lay Summary}{\LARGE}}
   {\end{frontenv}\newpage}

%% Define environments for linking to Isabelle lemmas and definitions
%% See amsthm doc section 4.3 for style reference
\newtheoremstyle{isalemma}%
  {}{}%
  {\upshape}{}%
  {\bfseries}{}%
  {0em}{}
\theoremstyle{isalemma}
\newtheorem{lemma}{Isabelle Lemma}[section]
\newenvironment{isalemma}[2][]%
  {\begin{lemma}[#1]\label{#2}\leavevmode\begin{isabelle}}%
  {\end{isabelle}\end{lemma}}
\newtheorem{defn}{Isabelle Definition}[section]
\newenvironment{isadef}[2][]%
  {\begin{defn}[#1]\label{#2}\leavevmode\begin{isabelle}}%
  {\end{isabelle}\end{defn}}
\newenvironment{isadefraw}[2][]%
  {\begin{defn}[#1]\label{#2}\leavevmode\begin{trivlist}\par\item\relax}%
  {\end{trivlist}\end{defn}}
