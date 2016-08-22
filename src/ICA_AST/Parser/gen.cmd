"../yc/YC.fslex.exe" ICA.fsl --unicode -o ICA.Lexer.fs
"../yc/YC.YaccConstructor.exe" -i ICA.yrd -g "RNGLRGenerator -pos int -token string -module GS.ICA.Parser -translate true -table LALR -o ICA.Parser.fs" 
