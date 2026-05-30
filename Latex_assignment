\documentclass[fleqn]{article}

% --------------------------------------------------
% Language and page layout
% --------------------------------------------------
\usepackage[english]{babel}
\usepackage[
    a4paper,
    top=1.5cm,
    bottom=1.5cm,
    left=2cm,
    right=2cm,
    marginparwidth=1.75cm
]{geometry}

% --------------------------------------------------
% Mathematics
% --------------------------------------------------
\usepackage{amsmath, amssymb, mathtools, amsthm}
\usepackage{bbm}
\usepackage{dsfont}
\usepackage{gensymb}

\setlength{\mathindent}{3cm}

\newcommand{\indicator}[1]{\mathbbm{1}\!\left\{#1\right\}}

% --------------------------------------------------
% Figures, and captions
% --------------------------------------------------
\usepackage{graphicx}
\usepackage{wrapfig}
\usepackage{float}
\usepackage{subcaption}

\renewcommand{\arraystretch}{1.2}
\setlength{\tabcolsep}{8pt}

% --------------------------------------------------
% Text formatting
% --------------------------------------------------
\usepackage{xcolor}
\usepackage{setspace}
\usepackage{titlesec}

\setstretch{1.0}
\setlength{\parindent}{15pt}

\newcommand{\red}[1]{{\color{red}#1}}
\newcommand{\code}[1]{\texttt{#1}}

% --------------------------------------------------
% Section formatting
% --------------------------------------------------
\titlespacing*{\section}{0pt}{0pt}{3pt}
\titlespacing*{\subsection}{0pt}{0pt}{2pt}

\titleclass{\subsubsubsection}{straight}[\subsubsection]

\newcounter{subsubsubsection}[subsubsection]
\renewcommand{\thesubsubsubsection}
    {\thesubsubsection.\arabic{subsubsubsection}}

\titleformat{\subsubsubsection}
    {\normalfont\normalsize\bfseries}
    {\thesubsubsubsection}
    {1em}
    {}

\titlespacing*{\subsubsubsection}{0pt}{5pt}{5pt}

% --------------------------------------------------
% Algorithms
% --------------------------------------------------
\usepackage{algorithm}
\usepackage{algpseudocode}

% --------------------------------------------------
% Code listings
% --------------------------------------------------
\usepackage{listings}

\lstset{
    language=R,
    basicstyle=\ttfamily\small,
    keywordstyle=\color{blue}\bfseries,
    commentstyle=\color{green!50!black},
    stringstyle=\color{red},
    showstringspaces=false,
    numbers=left,
    numberstyle=\tiny\color{gray},
    frame=single,
    breaklines=true
}

% --------------------------------------------------
% References and hyperlinks
% --------------------------------------------------
\usepackage{natbib}
\usepackage[colorlinks=true, allcolors=black]{hyperref}

% --------------------------------------------------
% Table style
% --------------------------------------------------
\setlength{\abovecaptionskip}{2pt}
\usepackage{unicode-math}

\usepackage{siunitx}
\usepackage{xcolor}
\usepackage{booktabs,colortbl, array}
\usepackage{pgfplotstable}
\pgfplotsset{compat=1.8}

\definecolor{rulecolor}{RGB}{0,71,171}
\definecolor{tableheadcolor}{gray}{0.92}
% Following is taken from Werner: http://tex.stackexchange.com/a/33761/3061
% and modified for my needs
%
% Command \topline consists of a (slightly modified)
% \toprule followed by a \heavyrule rule of colour tableheadcolor
% (hence, 2 separate rules)
\newcommand{\topline}{ %
        \arrayrulecolor{rulecolor}\specialrule{0.1em}{\abovetopsep}{0pt}%
        \arrayrulecolor{tableheadcolor}\specialrule{\belowrulesep}{0pt}{0pt}%
        \arrayrulecolor{rulecolor}}
% Command \midline consists of 3 rules (top colour tableheadcolor, middle colour black, bottom colour white)
\newcommand{\midtopline}{ %
        \arrayrulecolor{tableheadcolor}\specialrule{\aboverulesep}{0pt}{0pt}%
        \arrayrulecolor{rulecolor}\specialrule{\lightrulewidth}{0pt}{0pt}%
        \arrayrulecolor{white}\specialrule{\belowrulesep}{0pt}{0pt}%
        \arrayrulecolor{rulecolor}}
% Command \bottomline consists of 2 rules (top colour
\newcommand{\bottomline}{ %
        \arrayrulecolor{white}\specialrule{\aboverulesep}{0pt}{0pt}%
        \arrayrulecolor{rulecolor} %
        \specialrule{\heavyrulewidth}{0pt}{\belowbottomsep}}%


\newcommand{\midheader}[2]{%
        \midrule\topmidheader{#1}{#2}}
\newcommand\topmidheader[2]{\multicolumn{#1}{c}{\textsc{#2}}\\%
                \addlinespace[0.5ex]}

\pgfplotstableset{normal/.style ={%
        header=true,
        string type,
        column type=l,
        every odd row/.style={
            before row=
        },
        every head row/.style={
            before row={\topline\rowcolor{tableheadcolor}},
            after row={\midtopline}
        },
        every last row/.style={
            after row=\bottomline
        },
        col sep=&,
        row sep=\\
    }
}

% --------------------------------------------------
% Document starts here
% --------------------------------------------------
\begin{document}

\begin{center}
    \includegraphics[width=\textwidth]{rug logo (1) (2) (1).jpeg}
\end{center}

\newcommand{\HRule}{\rule{\linewidth}{0.5mm}}

\begin{center}

    \textsc{\Large Applied Microeconometrics}\\[0.5cm]

    \HRule \\[0.4cm]
    {\huge \bfseries Group Assignment}\\
    \HRule \\[0.4cm]

    \vspace{0.5cm}

    {\large \emph{Authors:}}\\[0.2cm]
    {\large \textsc{Quinten Salamons}, \textsc{s???????}}\\
    {\large \textsc{Niels Huijbregsen}, \textsc{s???????}}\\
    {\large \textsc{Juliën Stephan}, \textsc{s4887298}}

    \vfill

    {\large Version: \today}

\end{center}

\newpage

\section{Introduction}


\section{Model and estimators}
\subsection{Static panel data model}
We consider a static panel data model with both time-varying and time-invariant regressors. The model is given by 
\[ y_{it} = \beta_u x_{u,it} + \beta_c x_{c,it} + \gamma_u z_{u,i} + \gamma_c z_{c,i} + c_i + u_{it}, \]
where $i=1,\ldots, N$ indexes individuals and $t=1,\ldots, T$ indexes time. The variable $c_i$ denotes an unobserved individual-specific effect, while $u_{it}$ is an idiosyncratic error term. The regressors are divided into four groups, as shown in Table~\ref{tab:classification-regressors}. Here, exogenous means uncorrelated with the individual effect $c_i$, while endogenous means correlated with $c_i$. 


This distinction is important because the correlation between regressors and unobserved individual effects \setlength{\columnsep}{20pt}
\begin{wraptable}{r}{0.43\textwidth}
\vspace{-10pt}
\centering
\caption{Classification of the regressors}
\label{tab:classification-regressors}
\resizebox{\linewidth}{!}{%
\pgfplotstabletypeset[normal]{
Variable     & Time dimension         & Relation with $c_i$  \\
$x_u$        & time-varying           & exogenous \\
$x_c$        & time-varying           & endogenous \\
$z_u$        & time-invariant         & exogenous \\
$z_c$        & time-invariant         & endogenous \\
}
}
\vspace{-10pt}
\end{wraptable} determines which panel data estimators are appropriate. If regressors are correlated with $c_i$, estimators that treat $c_i$ as random and independent of the regressors will generally be biased. Time-invariant regressors create an additional challenge in panel data analysis because some commonly used methods rely exclusively on within-individual variation over time. Since time-invariant variables do not vary across periods for a given individual, their effects may not be identifiable under such transformations.
\\
\subsection{Estimators}
\subsubsection{Pooled ordinary least squares}
As a first benchmark, we estimate the model using pooled ordinary least squares (POLS). Let \\$w_{it} = (x_{u,it}, x_{c,it}, z_{u,i}, z_{c,i})'$ denote the vector of regressors. Pooled OLS estimates the parameter vector by minimizing the sum of squared residuals over all observations, treating the model as a standard cross-sectional regression. In doing so, it ignores the panel structure and absorbs the individual effect $c_i$ into the composite error term $v_{it} = c_i + u_{it}$. Consistency therefore requires $E(w_{it}v_{it}) = 0$. Since $x_{c,it}$ and $z_{c,i}$ are correlated with $c_i$ by construction, this condition is violated, implying that pooled OLS is generally biased and inconsistent. Nevertheless, it provides a useful baseline against which the panel estimators can be compared.

\subsubsection{Fixed effects}
The second estimator is the fixed effects (FE) estimator. FE removes the individual-specific $c_i$ by applying the within transformation, which subtracts the individual means from each variable:
\[
y_{it}-\bar y_i = \beta_u(x_{u,it}-\bar x_{u,i}) + \beta_c(x_{c,it}-\bar x_{c,i}) + (u_{it}-\bar u_i).
\]
The time-invariant variables $z_{u,i}$, $z_{c,i}$ and $c_i$ disappeared because their values do not vary over time, so theit individual means equal the variables themselves.  

\subsubsection{Random effects}
The third estimator is the random effects (RE) estimator. Random effects exploits the error-components structure $(v_{it}=c_i+u_{it})$ and applies generalized least squares to account for the covariance induced by the individual effect. The estimator can be viewed as a quasi-demeaning transformation,
\[
y_{it}-\theta \bar y_i=(w_{it}-\theta \bar w_i)'\delta +(u_{it}-\theta \bar u_i),
\]
where $\theta$ depends on the variance components of $c_i$ and $u_{it}$. Unlike fixed effects, RE retains variation in time-invariant regressors and therefore allows estimation of $\gamma_u$ and $\gamma_c$. However, consistency requires the strict exogeneity condition $E(c_i \mid w_{it}) = 0$ for all $t$. This assumption is violated in our simulation design for $x_{c,it}$ and $z_{c,i}$, so the RE estimator is expected to be biased in the presence of endogenous regressors.

\subsubsection{Correlated random effects}

\subsubsection{Hausman-Taylor}



\section{Simulation design}


\section{Performance measures}


\section{Results}


\section{Conclusion}


\end{document}
