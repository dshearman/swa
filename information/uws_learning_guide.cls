% UWS Learning Guide Class
% 
% (c) Laurence A. F. Park
% LPPL LaTeX Public Project License
%  
\NeedsTeXFormat{LaTeX2e}[1994/06/01]
\ProvidesClass{uws_learning_guide}
  [2013/12/10 v0.01 LaTeX class for UWS learning guide style]

\LoadClass[9pt,a4paper]{extarticle}


\def\unitimage#1{\def\@unitimage{#1}}
\def\unitnumber#1{\def\@unitnumber{#1}}
\def\unitname#1{\def\@unitname{#1}}
\def\unitlevel#1{\def\@unitlevel{#1}}
\def\teachingsession#1{\def\@teachingsession{#1}}
\def\teachingyear#1{\def\@teachingyear{#1}}

\def\printunitname{\@unitname}
\def\printunitnumber{\@unitnumber}
\def\printteachingsession{\@teachingsession}
\def\printteachingyear{\@teachingyear}


\RequirePackage[top=2cm, bottom=2cm, left=2.5cm, right=2.5cm]{geometry}
\RequirePackage{tabularx}
%\RequirePackage{ltablex}
\RequirePackage{fontspec}
%\setmainfont[Mapping=tex-text]{Times New Roman}
%\setmainfont[Mapping=tex-text]{Hoefler Text}
\setmainfont[Mapping=tex-text]{Optima}
%\setmainfont[Mapping=tex-text]{Arial}
%\setsansfont[Mapping=tex-text]{Arial Black}
\setmonofont[Mapping=tex-text]{Courier New}

\RequirePackage{setspace}
\RequirePackage{pdflscape}

\RequirePackage{parskip}
\setlength{\parskip}{\medskipamount}
%\newcommand{\@minipagerestore}{\setlength{\parskip}{\medskipamount}}

%\RequirePackage{multirow}


\RequirePackage{fancyhdr}
\pagestyle{fancy}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0.4pt}


\RequirePackage{xcolor,colortbl}
\definecolor{tableshade}{rgb}{0.77,0.85,94}
\definecolor{textshade}{rgb}{0.09,0.21,0.36}

\RequirePackage{hyperref}
%\hypersetup{%
%  colorlinks=false,% hyperlinks will be black
%  linkbordercolor=blue,% hyperlink borders will be red
%  pdfborderstyle={/S/U/W 1}% border style will be underline of width 1pt
%}

\newcommand{\HRule}{\rule{\linewidth}{0.2mm}}



\RequirePackage{titlesec}
%\titleformat{\section}{\Huge\sffamily}{\thesection.}{.5em}{}[\titlerule]
\titleformat{\section}{\color{textshade}\Huge\bfseries}{\thesection.}{0.5em}{}[\titlerule]
\titleformat{\subsection}{\LARGE\bfseries\color{textshade}}{}{0em}{}
\titleformat{\subsubsection}{\Large\itshape\color{textshade}}{}{0em}{}
\newcommand{\texttitle}[1]{{\textcolor{textshade}{\bfseries #1}}}

\RequirePackage[toc]{multitoc}
\renewcommand*{\multicolumntoc}{2}
\setlength{\columnseprule}{1pt}


\newenvironment{texttable}{
  \tabularx{\textwidth}{|>{\raggedright\columncolor{tableshade}}p{3cm}|>{\raggedright\let\\\tabularnewline}X|}
    \hline
  }{\endtabularx}

\newcommand{\texttablerow}[2]{
  \texttitle{#1} & 
  \setlength{\parskip}{\medskipamount} 
  #2 \\ 
  \hline
}

\newcommand{\texttabletitle}[2]{
  \rowcolor{tableshade}
  \texttitle{#1} & \texttitle{#2} \\
  \hline
}

\newenvironment{teachingactivities}{
  \tabularx{\textwidth}{|>{\raggedright\columncolor{tableshade}}p{3cm}|>{\raggedright\let\\\tabularnewline}X|}
    \hline
  }{\endtabularx}



%\font\webdings = "Webdings"
%\font\wingdings = "Wingdings"
\newfontfamily{\webdings}[Scale=1.2]{Webdings}
\newfontfamily{\wingdings}[Scale=1.2]{Wingdings}

\newcommand{\activitylogo}{{\webdings\XeTeXglyph84}}
\newcommand{\checklistlogo}{{\wingdings\XeTeXglyph59}}
\newcommand{\deadlinelogo}{{\webdings\XeTeXglyph136}}
\newcommand{\handoutlogo}{{\wingdings\XeTeXglyph21}}
\newcommand{\hintlogo}{{\webdings\XeTeXglyph179}}
\newcommand{\informationlogo}{{\webdings\XeTeXglyph76}}
\newcommand{\onlineactivitylogo}{{\wingdings\XeTeXglyph27}}
\newcommand{\readinglogo}{{\webdings\XeTeXglyph138}}
\newcommand{\toolkitlogo}{{\webdings\XeTeXglyph35}}
\newcommand{\warninglogo}{{\webdings\XeTeXglyph91}}


\newcommand{\lgtitlepage}{
\lhead{\@unitnumber~\@unitname}
\chead{}
\rhead{\@teachingsession, \@teachingyear}
\lfoot{}
\cfoot{}
\rfoot{\thepage}
\begin{titlepage}
 
\begin{flushright}
\scalebox{0.2}{\includegraphics{uws_cymk.pdf}}
\end{flushright}
\vspace{2em}
\fontspec[Scale=1.5]{Optima} %Times New Roman}
{\huge \texttitle{\@unitnumber~\@unitname}} \\
\HRule \\[1em]
{\Large School of Computing, Engineering and Mathematics}%\\[1em]
%\scalebox{0.57}{\includegraphics{cover-image}} \\[1em]
\begin{center}
%\scalebox{0.55}{\includegraphics{768px-Internet_map_1024}}
\includegraphics[width=\textwidth]{\@unitimage} %\\[1em]
\end{center} %\\[1em]
\vspace{1em}
\begin{flushright}
%\fontsize{40}{40}
\fontspec[Scale=1.5]{Optima} %{Times New Roman}
{\Huge  Learning Guide} \\[1em]
{\Huge \@teachingsession~\@teachingyear} \\[1em]
\end{flushright}
\fontspec[Scale=1]{Optima} %{Times New Roman}

\vfill
{\small\itshape
Level: \@unitlevel,  Edition: \@teachingsession{} \@teachingyear{}. \\
© Copyright: University of Western Sydney, \@teachingyear{}. \\
No part of this publication may be reproduced or transmitted in any
form or by any means, electronic or mechanical, including
photocopying, recording, or by any information storage and retrieval
system, without the prior written permission from the Dean, School of
Computing, Engineering and Mathematics. Copyright for acknowledged
materials reproduced herein is retained by the copyright holder. All
readings in this publication are copied under licence in accordance
with Part VB of the Copyright Act 1968. 


Cover image by The Opte Project taken from
\url{http://commons.wikimedia.org/wiki/File:Internet_map_1024.jpg} is
distributed under the Creative Commons Attribution 2.5 Generic
license. Details of the licence can be found at
\url{http://creativecommons.org/licenses/by/2.5/deed.en}


}
%\vfill
\noindent
\HRule
 
\end{titlepage}}


\newcommand{\lgkey}{
\begin{tabularx}{\textwidth}{|>{\columncolor{tableshade}}p{3cm}|>{\raggedright\let\\\tabularnewline}X|}
\hline
\rowcolor{tableshade}
\multicolumn{2}{|l|}{\texttitle{HOW TO USE THIS LEARNING GUIDE}} \\
\hline
Icon Key \newline
\noindent
\begin{tabular}{l>{\raggedright\let\\\tabularnewline}p{6em}}
\activitylogo{} & Activity \\
\checklistlogo{} & Checklist \\
\deadlinelogo{} & Deadline \\
\handoutlogo{} & Handout \\
\hintlogo{} & Hint \\
\informationlogo{} & Important information \\
\onlineactivitylogo{} & Online activity \\
\readinglogo{} & Reading \\
\toolkitlogo{} & Toolkit \\
\warninglogo{} & Warning
\end{tabular} &
\setlength{\parskip}{\medskipamount}

  This learning guide supplements the unit outline and is designed to
  help you navigate through the unit. It will help you focus on what
  you need to do for classes and the various assessment tasks. You
  should consult the relevant section of the learning guide as you
  plan your study – it will highlight the main things that you should
  be getting out of the resources available and provide guidance on
  teaching activities and class preparation.

  The learning guide also offers some study tips to assist you in
  developing the skills and techniques of an effective learner at
  university level. In addition to acquiring information and skills
  relevant to this unit, you should also focus on developing the
  habits and tools of a successful university student. As an adult
  learner you need to take control of your own learning and ensure
  your own success.

  This learning guide is specifically designed to help you achieve
  this.  A standard set of icons is used throughout the learning guide
  to make navigation easier. Use the icons to quickly identify
  important information, things you need to do and hints for doing
  them. \\
\hline
\end{tabularx}}
