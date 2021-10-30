\documentclass{scrartcl}
\usepackage{mathtools}
\usepackage{graphicx}
\usepackage[backend=biber,style=numeric]{biblatex}
\addbibresource{sicp.bib}
%include polycode.fmt

\begin{document}
\title{Learn Functional Programming with Haskell}
\author{Oliver Krischer}
\maketitle
\begin{abstract}
  Working through the classical book \citetitle{sicp96} \cite{sicp96} using \emph{Haskell}
\end{abstract}
\tableofcontents
\listoffigures

\section{Building Abstractions with Procedures}
\begin{code}
import Criterion.Main ( defaultMain, bench, bgroup, whnf )
\end{code}

\subsection{The Elements of Programming}
\subsubsection{Procedures as Black-Box Abstractions}

In this example we are including definitions for subroutines inside the main function, in order to keep the user interface clean.
We also use \emph{lexical scoping}: all references to the input value inside the subroutines (in this case |x|) receive their value directly from the main argument.

\begin{code}
sqrtHeron :: Double -> Double 
sqrtHeron x = iter 1
   where 
       satisfies guess = abs (guess^2 - x) < 0.001
       improve guess = (guess + (x / guess)) / 2
       iter guess =
            if satisfies guess then guess
            else iter $ improve guess
\end{code}

\subsection{Procedures and the Processes They Generate}
\subsubsection{Linear Recursion and Iteration}

The \emph{factorial function} is defined by the following \emph{recurrence relation}:
\begin{align*}
    1! &= 1 \\ n! &= n \cdot (n-1)!
\end{align*}

A straightforward implementation, resulting in a \emph{recursive process}:

\begin{code}
facRec :: Integer -> Integer
facRec n = case n of
  1 -> 1
  n -> n * facRec (n-1)
\end{code}

In contrast, here is an implementation which leads to an \emph{iterative process}: we use the concept of \emph{accumulation} in which we `store' the running product in a parameter of the iteration function:

\begin{code}
facIter :: Integer -> Integer
facIter = iter 1
  where
    iter p n = case n of
      1 -> p
      n -> iter (p*n) (n-1)
\end{code}

Observe, that we have a \emph{recursive function definition} in \textbf{both cases}, as this is the standard way of \emph{looping} in a pure functional programming language.
But the resulting \emph{computing process} differs in each case:
\begin{description}
\item[recursive:]
The program needs to keep track of the operations to be performed later on. Hence it has to store every frame of execution within the programs \emph{call stack}, which will grow and shrink during execution. This will lead to a space complexity of $\mathcal{O}(n)$ for this linear process.
\item[iterative:]
The program keeps track of the process with a fixed number of \emph{values} (in our case |p| for the running product and |n| for decreasing the input value), which we repeatedly recalculate. The stack size keeps constant, resulting in a space complexity of $\mathcal{O}(1)$.
\end{description}
Another way of understanding this, is to look at the actual recursive call of the function:
if there are no additional calculations to be performed (i.e. the resulting value is immediately returned), this will lead to an iterative process, provided the compiler is able to recognize and optimize this kind of \emph{tail recursive} calls.

\subsubsection{Tree Recursion}

Let's have a look at another standard example for recurrence relations, the \emph{Fibonacci numbers}:
\begin{align*}
    Fib_0 &= 0 \\ Fib_1 &= 1 \\ Fib_n &= Fib_{n-1} + Fib_{n-2}
\end{align*}

This translates directly into recursive code:

\begin{code}
fibRec :: Integer -> Integer
fibRec n
  | n < 2 = n
  | otherwise = fibRec (n-1) + fibRec (n-2)
\end{code}

With that recursion expression we are calling the function twice for every $n>1$, which leads to an exponential growth of calculation steps.
The resulting process looks like a tree, in which the branches split into two at each level.
In general, the number of steps required by a \emph{tree-recursive} process will be proportional to the number of nodes in the tree, while the space required will be proportional to the maximum depth of the tree.

\begin{code}
fibIter :: Integer -> Integer
fibIter = iter 0 1
  where
    iter a b n = case n of
      1 -> b
      n -> iter b (a+b) (n-1)
\end{code}

Here we have created an iterative implementation, \emph{accumulating} the current sum of the last two numbers.

\begin{figure}
  \caption{Benchmarking Fibonacci number generation}
  \centering
    \includegraphics[width=0.9\textwidth]{img/fibonacci.png}
\end{figure}

\begin{code}
main :: IO ()
main = defaultMain 
  [bgroup "fibonacci"
    [ bench "fibRec"  $ whnf fibRec  20
    , bench "fibIter" $ whnf fibIter 20]]
\end{code}

\printbibliography

\end{document}