% UWS Exam Package
% 
% (c) Laurence A. F. Park
% LPPL LaTeX Public Project License
%  
\NeedsTeXFormat{LaTeX2e}[1994/06/01]
\ProvidesClass{uwsexam}
  [2013/09/04 v0.01 LaTeX package for UWS exam style]

\LoadClass[a4paper,12pt]{article}

% set up answer visibility
\newif\if@showanswers
%% Note that showmarks does nothing yet.
\newif\if@showmarks
\DeclareOption{showanswers}{\@showanswerstrue}
\DeclareOption{hideanswers}{\@showanswersfalse}
\DeclareOption{showmarks}{\@showmarkstrue}
\DeclareOption{hidemarks}{\@showmarksfalse}
\ExecuteOptions{showanswers,showmarks}

\ProcessOptions

\if@showanswers
  \newcommand{\showanswers}{true}
\else
  \newcommand{\showanswers}{false}
\fi

\if@showmarks
  \newcommand{\showmarks}{true}
\else
  \newcommand{\showmarks}{false}
\fi

\newcommand{\testtitle}[1]{\gdef\@testtitle{#1}}%
\newcommand{\unitsession}[1]{\gdef\@unitsession{#1}}%
\newcommand{\unitname}[1]{\gdef\@unitname{#1}}%
\newcommand{\unitcode}[1]{\gdef\@unitcode{#1}}%
\newcommand{\testtime}[1]{\gdef\@testtime{#1}}%
\newcommand{\questioncount}[1]{\gdef\@questioncount{#1}}%
\newcommand{\lecturername}[1]{\gdef\@lecturername{#1}}%
\newcommand{\studentinstructions}[1]{\gdef\@studentinstructions{#1}}%




%\RequirePackage{euler}	
% Use arial math font
\RequirePackage{arev}	

% Use arial for text
\RequirePackage[no-math]{fontspec}
\setmainfont[Mapping=tex-text]{Arial}
\setsansfont[Mapping=tex-text]{Arial Black}
\setmonofont[Mapping=tex-text]{Courier}

%\def\begincomment#1\endcomment{} 

\RequirePackage{amssymb}
\RequirePackage{amsmath}
\RequirePackage{ifthen}

% \RequirePackage{versions}
% %\includeversion{ANSWERS}
% \excludeversion{ANSWERS}

\RequirePackage{array}

\RequirePackage{graphics}

\RequirePackage{ifthen}
\RequirePackage[hmargin={2.5cm,2.5cm},vmargin={3cm,3cm} ]{geometry} 

\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{enumerate}
\RequirePackage{fancyhdr,lastpage}
\RequirePackage{verbatim}
\RequirePackage{multirow}
\RequirePackage{booktabs}




% set up mark counters
\newcounter{totalmarks}
\newcounter{questionmarks}
\newcounter{currentsubquestion}
\newcounter{subquestionmarks1}
\newcounter{subquestionmarks2}
\newcounter{subquestionmarks3}
\newcounter{subquestionmarks4}
\newcounter{subquestionmarks5}

\newcommand\xmar[1]{{(1)}}

%\def\pgfsysdriver{pgfsys-dvipdfm.def}
\RequirePackage{tikz}
\newcommand\xmark[1]{%
  \showxmark{#1}%
  %\addtocounter{questionmarks}{#1}
  \addtocounter{subquestionmarks\arabic{currentsubquestion}}{#1}
  %\refstepcounter{questionmarks}
}

\newcommand\showxmark[1]{%
  \begin{tikzpicture}%
   \node[draw,circle,inner sep=1pt, fill=black, text=white]  {{\scriptsize #1}};%
  \end{tikzpicture}}

\newcommand{\checkme}{
  \textbf{CHECK ME} $\rightarrow$~}

\pagestyle{fancy} 
%\fancyhf{} 
\cfoot{Page \thepage\ of \pageref{LastPage}}
\lhead{\@unitcode~\@unitname}
\rhead{\@testtitle}
%\renewcommand\headrulewidth{0pt} % Removes funny header line 

\newcommand{\testend}{
  \answersection{
    \begin{flushright}
      \color{xgray}
      \checkme Total: \arabic{totalmarks} marks
    \end{flushright}
  }
  \begin{center}\textbf{\MakeUppercase{End of Examination Paper}}
  \end{center}}


\newcounter{questionnumber}
\setcounter{questionnumber}{0}

\setlength{\parindent}{0pt}
\setlength{\parskip}{1em}

\newcommand{\answersection}[1]{
  \ifthenelse{\equal{\showanswers}{true}}{#1}{}
}

\newcommand{\testsection}[1]{
  \ifthenelse{\equal{\showanswers}{true}}{}{#1}
}


\newcommand{\displayanswer}[2]{
  \ifthenelse{\equal{\showanswers}{true}}{#1}{#2}}

\newcommand{\questiontext}[2]{
  \begin{minipage}[t]{#1\textwidth}%
    #2%
  \end{minipage}%
}

%\newcommand{\question}[2]{\textbf{#1} (#2)
%  \setcounter{questionnumber}{0}}

\newcommand{\question}[2]{\subsection*{#1 (#2)}
  \vspace{-3em}
  \setcounter{questionnumber}{0}}

% set the size of the letterbox
\setlength{\fboxsep}{10pt}

\newcommand{\emptyanswerbox}[1]{
  \framebox{\phantom{#1}}
}

\newcommand{\answerbox}[1]{
  \framebox[3em]{#1}
}

% \newcommand{\workingbox}[1]{
%   \fbox{\begin{minipage}{\textwidth}
%       #1
%     \end{minipage}}
% }

% \newcommand{\workingbox}[1]{\begin{minipage}{\textwidth}
%     #1 \end{minipage}}

\newcommand{\HRule}{\rule{\linewidth}{0.2mm}}

\RequirePackage{color}
\definecolor{xgray}{rgb}{0.4,0.4,0.4}
\definecolor{xblack}{rgb}{0,0,0}

\newenvironment{workingbox}{
  % \begin{ANSWERS}
  \ifthenelse{\equal{\showanswers}{true}}{}{\comment}
  \HRule \\
  %\color{xgray}
  Answer:
}{
%\HRule
  \color{xblack}
 \ifthenelse{\equal{\showanswers}{true}}{}{\endcomment}
% \vspace{2em}
}



\newcommand{\questionpagebreak}{%
  \ifthenelse{\equal{\showanswers}{false}}{\pagebreak}{}
}


\newcommand{\answerboxe}[2]{
  \ifthenelse{\equal{\showanswers}{false}}{
     \hspace{-#2}\fbox{\parbox[t][#1][s]{1.0\textwidth}{~}}
  }{}
}


%\newenvironment{workingbox}{\comment}{\endcomment}

\newenvironment{workingfigure}{}{}



\newcommand{\emptyworkingbox}[2]{
 \fbox{\parbox[t][#2][s]{1.0\textwidth}{~}}
   %\fbox{\parbox[t][#2][s]{1.0\textwidth}{#1}}
  %\vphantom{\mbox{#1}}
}


\newcommand{\letterbox}[1]{
  \hfill\displayanswer{\answerbox{#1}}{\emptyanswerbox{M}}
}

\newcommand{\tfquestion}[2]{
  \noindent
  \addtocounter{questionnumber}{1}
  \displayanswer{\answerbox{#2}}{\emptyanswerbox{#2}}
  \quad \arabic{questionnumber}. \questiontext{0.85}{#1}
  \vspace{1em}
}

\newcommand{\mcquestion}[2]{
  \noindent
  \addtocounter{questionnumber}{1}
  \displayanswer{\answerbox{#2}}{\emptyanswerbox{#2}}
  \quad \arabic{questionnumber}. \questiontext{0.85}{#1}
  \vspace{1em}
}

\newcommand{\pquestion}[3]{
  \noindent
  \addtocounter{questionnumber}{1}
  \arabic{questionnumber}. \questiontext{0.95}{#1}
  \vspace{1em}
 %\fbox{\parbox{\textwidth}{#2}}
 \displayanswer{\workingbox{#2}}{\emptyworkingbox{#2}{#3}}
  %\vspace{1em}
}



\newenvironment{equestiontext}[1]{
  \begin{minipage}[t]{#1\textwidth}}{%
  \end{minipage}}


\newenvironment{xquestion}{
  \noindent
  \addtocounter{questionnumber}{1}
  %\arabic{questionnumber}.
  \alph{questionnumber}.
  \begin{equestiontext}{0.95}}{
 \end{equestiontext}}


\newenvironment{questions}[1][a.]{
  \noindent
  \addtocounter{questionnumber}{1}
  \setcounter{currentsubquestion}{0}
  % \arabic{questionnumber}.
  \begin{enumerate}[#1]}{
  \end{enumerate}
  \answersection{
    \combinesubquestionmarks
    \printquestionmarks
    \resetsubquestionmarks
  }
}

\newcommand{\combinesubquestionmarks}{
  \setcounter{questionmarks}{0}  
  \addtocounter{questionmarks}{\arabic{subquestionmarks1}}
  \addtocounter{questionmarks}{\arabic{subquestionmarks2}}
  \addtocounter{questionmarks}{\arabic{subquestionmarks3}}
  \addtocounter{questionmarks}{\arabic{subquestionmarks4}}
  \addtocounter{questionmarks}{\arabic{subquestionmarks5}}
  \addtocounter{totalmarks}{\arabic{questionmarks}}
}

\newcommand{\resetsubquestionmarks}{
  \setcounter{subquestionmarks1}{0}
  \setcounter{subquestionmarks2}{0}
  \setcounter{subquestionmarks3}{0}
  \setcounter{subquestionmarks4}{0}
  \setcounter{subquestionmarks5}{0}
}


\newcommand{\printquestionmarks}{
  \begin{flushright}
    %\color{xgray}
    \checkme
    \ifthenelse{\equal{\arabic{currentsubquestion}}{1}}{}{\arabic{subquestionmarks1}}%
    \ifthenelse{\equal{\arabic{subquestionmarks2}}{0}}{}{ + \arabic{subquestionmarks2}}%
    \ifthenelse{\equal{\arabic{subquestionmarks3}}{0}}{}{ + \arabic{subquestionmarks3}}%
    \ifthenelse{\equal{\arabic{subquestionmarks4}}{0}}{}{ + \arabic{subquestionmarks4}}%
    \ifthenelse{\equal{\arabic{subquestionmarks5}}{0}}{}{ + \arabic{subquestionmarks5}}%
    \ifthenelse{\equal{\arabic{currentsubquestion}}{1}}{}{ = }%
    \arabic{questionmarks} marks
  \end{flushright}
}


\newcommand{\squestion}{
  \stepcounter{currentsubquestion}
  \item
}

\newcommand{\marknote}[1]{#1}


% \newenvironment{\xquestion}{
%   \noindent
%   \addtocounter{questionnumber}{1}
%   \arabic{questionnumber}. \questiontext{0.95}{#1}
%   \vspace{1em}
%  %\fbox{\parbox{\textwidth}{#2}}
%  \displayanswer{\fbox{\begin{workingbox} #2 \end{workingbox}}}{}
%  %\vspace{1em}
% }

\newcommand{\pnquestion}[2]{
  #1

  Answer:

  #2
}


%\newenviromnent{\pnquestion}{
% set page margins
%\RequirePackage[margin=2cm]{geometry} 

\newcommand{\qmark}[1]{\quad\textbf{(#1)}}


\newcommand{\coversheet}{%
%\begin{titlepage}

\thispagestyle{empty}

~\vspace{-5em}

\begin{flushright}
\scalebox{0.2}{\includegraphics{../../information/uws_cymk}} \\[1em]
\end{flushright}

{\Large
\begin{center}
\textbf{\MakeUppercase{\@testtitle}} \\
\textbf{\MakeUppercase{\@unitsession}}
\end{center}
}
\vspace{-1.5em}
{\large
\begin{center}
\textbf{\MakeUppercase{School of Computing, Engineering and Mathematics}}
\end{center}
}
%\vspace{-2em}

\ifthenelse{\equal{\showanswers}{true}}{
\begin{center}
  {\Large \textbf{\MakeUppercase{Marking Guide}}}
\end{center}

{\normalsize The marks allocated are shown as \showxmark{$x$} after
  the required text, where $x$ is the number of marks allocated. Note
  this guide is to assist in marking. If a student provides a correct
  solution with complete working, the student should be awarded full
  marks for the question, even if it is different to the solution
  provided in this guide.}  }{}

\ifthenelse{\equal{\showmarks}{false}}{
\begin{center}
  {\Large \textbf{\MakeUppercase{Example Solutions}}}
\end{center}

{\normalsize The marks allocated are shown as \showxmark{$x$} after
  the required text, where $x$ is the number of marks allocated. Note
  this guide is to assist in marking. If a student provides a correct
  solution with complete working, the student should be awarded full
  marks for the question, even if it is different to the solution
  provided in this guide.}  }{}


\ifthenelse{\equal{\showanswers}{true}}{}{
\begin{center}
\renewcommand{\arraystretch}{1.7}
\begin{tabular}{|>{\raggedright}p{0.3\textwidth}|>{\raggedright\arraybackslash}p{0.6\textwidth}|}
\hline
\textbf{Student Family Name:} & \\
\hline
\textbf{Student Given Name/s:} & \\
\hline
\textbf{Student ID Number:} & \\
\hline
% \textbf{Tutorial Day and Time:} & \\
% \hline
% \textbf{Campus Enrolment:} & \\
% \hline
\textbf{Course:} & \\
\hline
%Lecturer & \\
%\hline
\end{tabular}
\end{center}
}



\begin{center}
\renewcommand{\arraystretch}{1.3}
\begin{tabular}{>{\raggedright}p{0.3\textwidth}>{\raggedright\arraybackslash}p{0.65\textwidth}}
\textbf{Unit Name:} & \@unitname \\
\textbf{Unit Number:} & \@unitcode \\
\textbf{Time Allowed:} & \@testtime \\
\textbf{Number of Questions:} & \@questioncount \\
\textbf{Total Number of Pages:} & \pageref{LastPage}, including exam cover sheet \\
\textbf{Lecturer's Name:} & \@lecturername
\end{tabular}
\renewcommand{\arraystretch}{1.3}
\end{center}


\testsection{
  \begin{center}
    \textbf{INSTRUCTIONS} \\
    \textbf{PLEASE READ CAREFULLY BEFORE PROCEEDING}
  \end{center}
  {\footnotesize
    \@studentinstructions
  }
  \begin{center}
    \underline{\textbf{DO NOT TAKE THIS PAPER FROM THE EXAMINATION ROOM}}
  \end{center}
}

%\end{titlepage}}
\newpage}

\newcommand{\studentinstructionstesttwo}{
\begin{enumerate}
\item Write your name and student number on the top of this examination paper and on ALL answer booklets.
\item All examinations include reading time, which allows you to spend some time at the start of the examination composing yourself, reading the examination paper, and planning which questions to answer, and how. We advise you to read the instructions and questions carefully, before you begin writing. \textbf{However, you are allowed to begin writing straight away if you wish.}
\item This is an ``open book'' examination. Any written or printed material may be brought into the exam.
\item Answer all questions in the answer booklet provided.
\item There are 64 marks available. Each question is worth 8 marks. All working must be shown to gain full marks.
\item Scientific, non-programmable hand held calculators are permitted.
\item You should answer 8 of the 9 questions. If all 9 questions are answered only the first 8 will be marked.
\end{enumerate}
}

\endinput
