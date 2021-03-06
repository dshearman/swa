% UWS Exam Package
% 
% (c) Laurence A. F. Park
% LPPL LaTeX Public Project License
%  
\NeedsTeXFormat{LaTeX2e}[1994/06/01]
\ProvidesClass{uwsexamTFS}
  [2013/09/04 v0.01 LaTeX package for UWS exam style]

\RequirePackage{kvoptions}
\DeclareStringOption[12pt]{fontsize}
\DeclareStringOption[a4paper]{papersize}
\ProcessKeyvalOptions{uwsexamTFS}\relax
\PassOptionsToClass{\uwsexamTFS@fontsize}{extarticle}
\PassOptionsToClass{\uwsexamTFS@papersize}{extarticle}

\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{extarticle}%
}
\ProcessOptions\relax

%\LoadClass[a4paper,17pt]{extarticle}
\LoadClass{extarticle}

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



\RequirePackage{tabularx}

%\RequirePackage{euler}	
% Use arial math font
\RequirePackage{arev}	

% Use arial for text
\RequirePackage[no-math]{fontspec}
\setmainfont[Mapping=tex-text]{Arial}
\setsansfont[Mapping=tex-text]{Arial Black}
\setmonofont[Mapping=tex-text]{Courier New Bold}

%\def\begincomment#1\endcomment{} 

\RequirePackage{amssymb}
\RequirePackage{amsmath}
\RequirePackage{ifthen}

% \RequirePackage{versions}
% %\includeversion{ANSWERS}
% \excludeversion{ANSWERS}

\RequirePackage{array}

\RequirePackage{graphicx}

\RequirePackage{ifthen}
%\RequirePackage[hmargin={1.5cm,1.5cm},vmargin={3cm,3cm}, landscape]{geometry} 
\RequirePackage[hmargin={1.5cm,1.5cm},vmargin={3cm,2.5cm}]{geometry} 

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
\cfoot{\footnotesize Page \thepage\ of \pageref{LastPage}}
\lhead{\footnotesize \@unitcode~\@unitname}
\rhead{\footnotesize \@testtitle}
%\renewcommand\headrulewidth{0pt} % Removes funny header line 

\fancypagestyle{firststyle}
{
   \renewcommand\headrulewidth{0pt} % Removes funny header line 
   \fancyhf{}
   %\fancyfoot[C]{\footnotesize Page \thepage\ of \pageref{LastPage}}
   \fancyfoot[R]{\footnotesize (No page number- coversheet only)}
}

\fancypagestyle{coverstyle}
{
   \renewcommand\headrulewidth{0pt} % Removes funny header line 
   \fancyhf{}
   \fancyfoot[C]{\footnotesize Page \thepage\ of \pageref{LastPage}}
   %\fancyfoot[R]{\footnotesize (No page number- coversheet only)}
}


\newcommand{\testend}{
  \answersection{
    \begin{flushright}
      \color{xgray}
      \checkme Total: \arabic{totalmarks} marks
    \end{flushright}
  }
  \begin{center}\textbf{\MakeUppercase{End of Exam Paper}}
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

%% allows first page to be unnumbered
\thispagestyle{coverstyle}

~\vspace{-5em}

%\begin{minipage}[b]{0.75\textwidth}%
\begin{minipage}[b]{\textwidth}%
\centering
%\begin{center}
%\textbf{\MakeUppercase{\@testtitle}} -
%\textbf{\MakeUppercase{\@unitsession}}
%\end{center}
%{\large
%Assessment and Graduation Unit }\\[0.5em]
\begin{center}
\includegraphics[scale=0.6]{WSU_Logo_RGB.pdf}
\end{center}
{\Large
%WESTERN SYDNEY UNIVERSITY \\[0.5em]
\textbf{\@testtitle} -- \@unitsession
} \\[1em]
{\large
%\begin{center}
%\textbf{\MakeUppercase{School of Computing, Engineering and Mathematics}}
\textbf{School of Computing, Engineering and Mathematics}
%\end{center}
}
%\vspace{-2em}
\end{minipage}%
%\hfill
%%\begin{flushright}
%\scalebox{0.14}{\includegraphics{../../information/uws_cymk}}
%%\end{flushright}



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
%\begin{center}
\renewcommand{\arraystretch}{1.7}
\begin{tabularx}{\textwidth}{|>{\raggedright}p{0.3\textwidth}|>{\raggedright\arraybackslash}X|}
\hline
%\multicolumn{2}{|c|}{\underline{\textbf{STUDENT DETAILS}}} \\[-0.5em]
\multicolumn{2}{|l|}{\footnotesize \textit{Complete your details in this section when instructed by the Exam Supervisor at the start of the exam.}} \\[-1em]
\multicolumn{2}{|l|}{\footnotesize \textit{You should also complete your details on any answer booklets provided.}} \\
\hline
\textbf{\MakeUppercase{Student Surname}:} & \\
\hline
\textbf{\MakeUppercase{Student First Name}:} & \\
\hline
\textbf{\MakeUppercase{Student ID}:} & \\
\hline
% \textbf{Tutorial Day and Time:} & \\
% \hline
% \textbf{Campus Enrolment:} & \\
% \hline
%\textbf{Course:} & \\
%\hline
%Lecturer & \\
%\hline
\end{tabularx}
%\end{center}
}



%\begin{center}
\renewcommand{\arraystretch}{1.7}
%\renewcommand{\arraystretch}{1.3}

\begin{tabularx}{\textwidth}{|>{\raggedright}p{0.3\textwidth}|>{\raggedright\arraybackslash}X|}
\hline
\multicolumn{2}{|c|}{\underline{\textbf{EXAM INSTRUCTIONS}}} \\[-0.5em]
\multicolumn{2}{|l|}{\footnotesize \textit{Read all the information below and follow any instructions carefully before proceeding.}} \\[-1em]
\multicolumn{2}{|l|}{\footnotesize \textit{This exam is printed on both sides of the paper – ensure you answer all the questions.}} \\[-1em]
\multicolumn{2}{|l|}{\footnotesize \textit{You may begin writing when instructed by the Exam Supervisor at the start of the exam.}} \\[-1em]
\multicolumn{2}{|l|}{\footnotesize \textit{Clearly indicate which question you are answering on any Examination Answer Booklets used.}} \\
\hline
\textbf{Unit Name:} & \@unitname \\
\hline
\textbf{Unit Number:} & \@unitcode \\
\hline
\textbf{Number of Questions:} & \@questioncount \\
\hline
\textbf{Total Number of Pages:} & \pageref{LastPage} \\
\hline
\textbf{Value of Questions:} & Exam questions are worth 8 marks, giving a total of 56 marks. \\
\hline
\textbf{Answering Questions:} & Answer all questions in the Examination Answer Booklet provided. \\
\hline
\textbf{Unit Coordinator:} & \@lecturername \\
\hline
\textbf{Time Allowed:} & \@testtime \\
\hline
\end{tabularx}
\renewcommand{\arraystretch}{1.3}
%\end{center}


%\begin{center}
\renewcommand{\arraystretch}{1.7}
%\renewcommand{\arraystretch}{1.3}

\begin{tabularx}{\textwidth}{|>{\raggedright\arraybackslash}X|}
\hline
\multicolumn{1}{|c|}{\underline{\textbf{RESOURCES ALLOWED}}} \\[-0.5em]
{\footnotesize \textit{Only the resources listed below are allowed in this exam.}} \\
\hline
All printed materials, books, handwritten or printed notes are allowed. \\
Any calculator which has the primary function of a calculator is allowed.  For example, calculators on mobile phones or similar electronic devices are not allowed.\\
%All printed materials, books, handwritten or printed notes are allowed. \\
\hline
\end{tabularx}
\renewcommand{\arraystretch}{1.3}
%\end{center}
\testsection{
%  \begin{center}
%    \textbf{INSTRUCTIONS} \\
%    \textbf{PLEASE READ CAREFULLY BEFORE PROCEEDING}
%  \end{center}
%  {\footnotesize
%    \@studentinstructions
%  }
\hfill
  \begin{center}
    \underline{\textbf{DO NOT TAKE THIS PAPER FROM THE EXAM ROOM}}
  \end{center}
}

%\end{titlepage}}
\newpage
\vspace*{\fill}
  \begin{center}
    \textbf{This page intentionally left blank}
  \end{center}
\vspace*{\fill}
\newpage
\clearpage
%% reset page numbering
%\setcounter{page}{1}
}

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
