BeginPackage["TweakLaTeX`"]
ToLaTeX::usage="Return a string representing a LaTeX expression tweaked the way this package likes.\nIt is also copied to clipboard.";
EvaluationToLaTeX::usage="EvaluationToLaTeX[expr__, separator_: \" = \"] converts an expresion and its evaluation to LaTeX.\nIt is also copied to clipboard.";
Headers::usage="A string with the additional headers you should need for your LaTeX document.";

PreviewLaTeX::usage="WIP. Preview a LaTeX formula.";

Begin["`Private`"]

Rules = {};
Headers = "";

(*Round differential in integrals -- requires commath *)
Headers = Headers <> "\\usepackage{commath}\n"
AppendTo[Rules, "\\int" ~~ x___ ~~ "\\, d" :> "\\int" <> x <> "\\, \\dif "]

(*Round e and i *)
Headers = Headers <> "\\newcommand{\\eu}{\\mathrm{e}}\n";
AppendTo[Rules, WordBoundary ~~ "e" ~~ WordBoundary :> "\\eu"]
Headers = Headers <> "\\newcommand{\\im}{\\mathrm{i}}\n";
AppendTo[Rules, WordBoundary ~~ "i" ~~ WordBoundary :> "\\im"]


(*TODO: Improve argument detection*)

(*Remove parenthesis in common functions of a (one lettered) variable*)
CommonFunctions = {"sin", "cos", "tan", "sinh", "cosh", "tanh"};
AppendTo[Rules, 
 "\\" ~~ f : Alternatives @@ CommonFunctions ~~ " (" ~~ x_ ~~ ")" :> 
  "\\" <> f <> "{" <> x <> "}"]
  
(*Do the same with greek letters*)
Letters = {"alpha", "beta", "gamma", "delta", "epsilon", "varepsilon", "zeta", "eta", "theta", "vartheta", "kappa", "lambda", "mu", "nu", "xi", "omicron", "pi", "varpi", "rho", "varrho", "sigma", "varsigma", "tau", "upsilon", "phi", "varphi", "chi", "psi", "omega"};
Letters = Join[Letters,Capitalize[Letters]]
AppendTo[Rules, 
 "\\" ~~ f : Alternatives @@ CommonFunctions ~~ " (\\" ~~ x:Letters ~~ " )" :> 
  "\\" <> f <> "{\\" <> x <> "}"]






ToLaTeX[f_] := Block[{s, out}, s = ToString[TeXForm[f]];
  (*Fold to avoid skipping anidated rules*)
  out = Fold[StringReplace, s, Rules];
  CopyToClipboard[out];
  out]

EvaluationToLaTeX[expr__, separator_: " = "] := Block[{out},
  out = ToLaTeX[HoldForm[expr]] <> separator <> ToLaTeX[expr];
  CopyToClipboard[out];
  out]
  
Attributes[EvaluationToLaTeX] = {HoldAllComplete}

template = "\\documentclass[border=2pt]{standalone}
 \\usepackage{amsmath}
 \\usepackage{varwidth}
 %%HEADERS%%
 \\begin{document}
 \\begin{varwidth}{\\linewidth}
 %%FORMULA%%
 \\end{varwidth}
 \\end{document}"

(*TODO: WIP. pdflatex runs with Mma's Path and fails due to its libraries being in path.*)
PreviewLaTeX[s_] := Block[{file}, dir = CreateDirectory[];
  WriteString[
   FileNameJoin[{dir, "preview.txt"}], 
   StringReplace[
    template, {"%%HEADERS%%" -> Headers, "%%FORMULA%%" -> s}], "\n"];
  Print[Import[FileNameJoin[{dir, "preview.txt"}]]];
  RunProcess[$SystemShell, All, 
   "cd " <> dir <> 
    " && mv preview.txt preview.tex && echo hola> test.txt && source \
~/.bashrc && pdflatex preview.tex && convert -density 300 preview.pdf \
-quality 300 preview.png\nexit\n"];
  Import[FileNameJoin[{dir, "preview.png"}]];
  DeleteDirectory[dir]]


End[]

EndPackage[]

