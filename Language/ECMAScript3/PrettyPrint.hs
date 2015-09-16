{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE CPP #-}

-- | Pretty-printing JavaScript.
module Language.ECMAScript3.PrettyPrint (Pretty (..)
                                        ,javaScript
                                        ,renderStatements
                                        ,renderExpression
                                        ,PP (..)
                                        ,unsafeInExprStmt
                                        ) where

import Text.PrettyPrint.Leijen hiding (Pretty)
import Language.ECMAScript3.Syntax
#if __GLASGOW_HASKELL__ > 708
import Prelude hiding (maybe, id, (<$>))
#else
import Prelude hiding (maybe, id)
#endif
import Data.Char
import Numeric

{-# DEPRECATED PP, javaScript, renderStatements, renderExpression "These interfaces are outdated and would be removed/hidden in version 1.0. Use the Pretty class instead." #-}

-- | A class of pretty-printable ECMAScript AST nodes. Will
-- pretty-print correct JavaScript given that the 'isValid' predicate
-- holds for the AST.
class Pretty a where
  -- | Pretty-print an ECMAScript AST node. Use 'render' or 'show' to
  -- convert 'Doc' to 'String'.
  prettyPrint :: a -> Doc

instance Pretty (JavaScript a) where
  prettyPrint (Script _ ss) = prettyPrint ss

instance Pretty [Statement a] where 
  prettyPrint = vcat . map prettyPrint

instance Pretty (Expression a) where 
  prettyPrint = ppExpression True

instance Pretty (Statement a) where 
  prettyPrint s = case s of
    BlockStmt _ ss -> asBlock ss
    EmptyStmt _ -> semi
    ExprStmt _ e | unsafeInExprStmt (e) -> parens (ppExpression True e) <> semi
    ExprStmt _ e | otherwise            -> ppExpression True e <> semi
    IfSingleStmt _ test cons -> text "if" <+> 
                                parens (ppExpression True test) </> 
                                (nest 3 $ prettyPrint cons)
    IfStmt _ test cons alt -> text "if" <+> parens (ppExpression True test) </>
                              (nest 3 $ prettyPrint cons) </> text "else"
                              <+> (nest 3 $ prettyPrint alt)
    SwitchStmt _ e cases ->
      text "switch" <+> parens (ppExpression True e) <$>
      braces (nest 3 (vcat (map prettyPrint cases)))
    WhileStmt _ test body -> text "while" <+> parens (ppExpression True test) </>
                             prettyPrint body
    ReturnStmt _ Nothing -> text "return" <> semi
    ReturnStmt _ (Just e) -> text "return" <+> ppExpression True e <> semi
    DoWhileStmt _ s e -> 
      text "do" </> 
      (prettyPrint s </> text "while" <+> parens (ppExpression True e)
       <> semi)
    BreakStmt _ Nothing ->  text "break" <> semi
    BreakStmt _ (Just label) -> text "break" <+> prettyPrint label <> semi
    ContinueStmt _ Nothing -> text "continue" <> semi
    ContinueStmt _ (Just label) -> text"continue" <+> prettyPrint label
                                   <> semi
    LabelledStmt _ label s -> prettyPrint label <> colon </> prettyPrint s
    ForInStmt p init e body -> 
      text "for" <+> 
      parens (prettyPrint init <+> text "in" <+> ppExpression True e) </> 
      prettyPrint body
    ForStmt _ init incr test body ->
      text "for" <+> 
      parens (prettyPrint init <> semi <+> maybe incr (ppExpression True) <> 
              semi <+> maybe test (ppExpression True)) </> 
      prettyPrint body
    TryStmt _ stmt mcatch mfinally ->
      text "try" </> inBlock stmt </> ppCatch </> ppFinally 
      where ppFinally = case mfinally of
              Nothing -> empty
              Just stmt -> text "finally" <> inBlock stmt
            ppCatch = case mcatch of
              Nothing -> empty
              Just cc -> prettyPrint cc
    ThrowStmt _ e -> text "throw" <+> ppExpression True e <> semi
    WithStmt _ e s -> text "with" <+> parens (ppExpression True e)
                      </> prettyPrint s
    VarDeclStmt _ decls ->
      text "var" <+> cat (punctuate comma (map (ppVarDecl True) decls))
      <> semi
    FunctionStmt _ name args body ->
      text "function" <+> prettyPrint name <> 
      parens (cat $ punctuate comma (map prettyPrint args)) <+>
      asBlock body

-- | A predicate to tell if the expression --when pretty-printed--
-- will begin with "function" or '{' and be thus unsafe to use in an
-- expression statement without wrapping it in '()'.
unsafeInExprStmt :: Expression a -> Bool
-- property: forall e. unsafeInExprStmt(e) <==> prettyPrint(e) begins
-- with "function" or '{'
unsafeInExprStmt = unsafeInExprStmt_ 15
  where unsafeInExprStmt_ prec e =
          case e of
            ObjectLit {} -> True
            DotRef _ obj _ | prec >= 1 -> unsafeInExprStmt_ 1 obj
            BracketRef _ obj _ | prec > 0 -> unsafeInExprStmt_ 1 obj
            UnaryAssignExpr a op lv | (op `elem` [PostfixInc, PostfixDec])
                                      && (prec > 3) -> unsafeLv 2 lv
            InfixExpr _ _ l _ | prec >= 5  -> unsafeInExprStmt_ 5 l
            CondExpr _ c _ _ | prec >= 12 -> unsafeInExprStmt_ 12 c
            AssignExpr _ _ lv _ | prec >= 13 -> unsafeLv 2 lv
            ListExpr _ (e:_) | prec >= 14 -> unsafeInExprStmt_ 14 e
            CallExpr _ e _ | prec >= 2 -> unsafeInExprStmt_ 2 e
            FuncExpr {} -> True
            _ -> False
        unsafeLv prec lv = case lv of
          LVar {} -> False
          LDot _ obj _ -> unsafeInExprStmt_ prec obj
          LBracket _ obj _ -> unsafeInExprStmt_ prec obj
            
instance Pretty (CatchClause a) where
  prettyPrint (CatchClause _ id s) =
    text "catch" <+> (parens.prettyPrint) id <+> inBlock s

instance Pretty (ForInit a) where 
  prettyPrint t = case t of
    NoInit     -> empty
    VarInit vs -> text "var"
                  <+> cat (punctuate comma $ map (ppVarDecl False) vs)
    ExprInit e -> ppExpression False e

instance Pretty (ForInInit a) where
  prettyPrint t = case t of
    ForInVar id  -> text "var" <+> prettyPrint id
    ForInLVal lv -> prettyPrint lv

instance Pretty (LValue a) where 
  prettyPrint lv = case lv of
    LVar _ x -> printIdentifierName x
    LDot _ e x -> ppObjInDotRef e ppMemberExpression <> text "." <> printIdentifierName x
    LBracket _ e1 e2 -> ppMemberExpression e1 <> 
                        brackets (ppExpression True e2)

instance Pretty (VarDecl a) where
  prettyPrint = ppVarDecl True

instance Pretty (CaseClause a) where
  prettyPrint c = case c of
    CaseClause _ e ss -> 
      text "case" <+> ppExpression True e <> colon </> nest 2 (prettyPrint ss)
    CaseDefault _ ss -> text "default:" </> nest 2 (prettyPrint ss)

instance Pretty InfixOp where 
   prettyPrint op = text $ case op of
     OpMul -> "*"
     OpDiv -> "/"
     OpMod -> "%" 
     OpAdd -> "+" 
     OpSub -> "-"
     OpLShift -> "<<"
     OpSpRShift -> ">>"
     OpZfRShift -> ">>>"
     OpLT -> "<"
     OpLEq -> "<="
     OpGT -> ">"
     OpGEq -> ">="
     OpIn -> "in"
     OpInstanceof -> "instanceof"
     OpEq -> "=="
     OpNEq -> "!="
     OpStrictEq -> "==="
     OpStrictNEq -> "!=="
     OpBAnd -> "&"
     OpBXor -> "^"
     OpBOr -> "|"
     OpLAnd -> "&&"
     OpLOr -> "||"

instance Pretty AssignOp where 
  prettyPrint op = text $ case op of
    OpAssign -> "="
    OpAssignAdd -> "+="
    OpAssignSub -> "-="
    OpAssignMul -> "*="
    OpAssignDiv -> "/="
    OpAssignMod -> "%="
    OpAssignLShift -> "<<="
    OpAssignSpRShift -> ">>="
    OpAssignZfRShift -> ">>>="
    OpAssignBAnd -> "&="
    OpAssignBXor -> "^="
    OpAssignBOr -> "|="

instance Pretty PrefixOp where 
  prettyPrint op = text $ case op of
    PrefixLNot -> "!"
    PrefixBNot -> "~"
    PrefixPlus -> "+"
    PrefixMinus -> "-"
    PrefixTypeof -> "typeof"
    PrefixVoid -> "void"
    PrefixDelete -> "delete"

instance Pretty (Prop a) where
  prettyPrint p = case p of
    PropId _ id -> prettyPrint id
    PropString _ str -> dquotes $ text $ jsEscape str
    PropNum _ n -> text (show n)

instance Pretty (Id a) where
  prettyPrint (Id _ str) = printIdentifierName str

class PP a where 
  pp :: a -> Doc

instance Pretty a => PP a where 
  pp = prettyPrint

-- | DEPRECATED: Use 'prettyPrint' instead! Renders a JavaScript
-- program as a document, the show instance of 'Doc' will pretty-print
-- it automatically
javaScript :: JavaScript a -> Doc
javaScript = prettyPrint

-- | DEPRECATED: Use 'prettyPrint' instead! Renders a list of
-- statements as a 'String'
renderStatements :: [Statement a] -> String
renderStatements = show . prettyPrint

-- | DEPRECATED: Use 'prettyPrint' instead! Renders a list of
-- statements as a 'String'
renderExpression :: Expression a -> String
renderExpression = show . prettyPrint

-- Displays the statement in { ... }, unless it is a block itself.
inBlock:: Statement a -> Doc
inBlock s@(BlockStmt _ _) = prettyPrint s
inBlock s                 = asBlock [s]

asBlock :: [Statement a] -> Doc
asBlock [] = lbrace <$$> rbrace
asBlock ss = lbrace <> line <> (indentBlock $ prettyPrint ss) <$$> rbrace

indentBlock :: Doc -> Doc
indentBlock = indent 3

ppVarDecl :: Bool -> VarDecl a -> Doc
ppVarDecl hasIn vd = case vd of
  VarDecl _ id Nothing  -> prettyPrint id
  VarDecl _ id (Just e) -> prettyPrint id <+> equals
                           <+> ppAssignmentExpression hasIn e

-- | Pretty prints a string assuming it's used as an identifier. Note
-- that per Spec 7.6 unicode escape sequences representing illegal
-- identifier characters are not allowed as well, so we do not
-- unicode-escape illegal characters in identifiers anymore.
printIdentifierName :: String -> Doc
printIdentifierName = text

-- Based on:
--   http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Guide:Literals
jsEscape:: String -> String
jsEscape "" = ""
jsEscape (ch:chs) = sel ch ++ jsEscape chs where
    sel '\b' = "\\b"
    sel '\f' = "\\f"
    sel '\n' = "\\n"
    sel '\r' = "\\r"
    sel '\t' = "\\t"
    sel '\v' = "\\v"
    sel '\'' = "\\'"
    sel '\"' = "\\\""
    sel '\\' = "\\\\"
    sel x    = [x]
    -- We don't have to do anything about \X, \x and \u escape sequences.

-- | Escapes a regular expression so that it can be parsed correctly afterwards
regexpEscape :: String -> String
regexpEscape = regexpEscapeChar True
  where regexpEscapeChar :: Bool -- ^ First char?
                         -> String -> String
        regexpEscapeChar first s = 
          case (s, first) of
            ("", True) -> "(?:)"
            ("", False)-> ""
            -- see spec 7.8.5, RegularExpressionFirstChar
            ("\\", _) -> "\\\\"
            ('\\':c:rest, _) -> '\\':c:(regexpEscapeChar False rest)
            ('/':rest, _) -> '\\':'/':regexpEscapeChar False rest
            ('*':rest, True) -> ('\\':'*':regexpEscapeChar False rest)
            (c:rest, _)   -> c:regexpEscapeChar False rest

-- 11.1
ppPrimaryExpression :: Expression a -> Doc
ppPrimaryExpression e = case e of
  ThisRef _ -> text "this"
  VarRef _ id -> prettyPrint id
  NullLit _ -> text "null"
  BoolLit _ True -> text "true"
  BoolLit _ False -> text "false"
  NumLit  _ n -> double n
  IntLit _ n ->  int n
  StringLit _ str -> dquotes $ text $ jsEscape str
  RegexpLit _ reg g ci -> text "/" <> (text (regexpEscape reg)) <> text "/" <>
                          (if g then text "g" else empty) <> 
                          (if ci then text "i" else empty)
  ArrayLit _ es -> list $ map (ppAssignmentExpression True) es
  ObjectLit _ xs -> encloseSep lbrace rbrace comma $ map ppField xs
    where ppField (f,v)= prettyPrint f <> colon <+> ppAssignmentExpression True v
  _ -> parens $ ppExpression True e

-- 11.2
ppMemberExpression :: Expression a -> Doc
ppMemberExpression e = case e of
  FuncExpr _ name params body -> 
    text "function" <+> maybe name (\n -> prettyPrint n <> space) <>
    parens (cat $ punctuate comma (map prettyPrint params)) <+>
    asBlock body
  DotRef _ obj id -> ppObjInDotRef obj ppMemberExpression <> text "." <> prettyPrint id
  BracketRef _ obj key -> 
    ppMemberExpression obj <> brackets (ppExpression True key)  
  NewExpr _ ctor args -> 
    text "new" <+> ppMemberExpression ctor <> ppArguments args
  _ -> ppPrimaryExpression e

ppCallExpression :: Expression a -> Doc
ppCallExpression e = case e of
  CallExpr _ f args -> ppCallExpression f <> ppArguments args
  DotRef _ obj id -> ppObjInDotRef obj ppCallExpression <> text "." <> prettyPrint id
  BracketRef _ obj key -> ppCallExpression obj
                          <> brackets (ppExpression True key)
  _ -> ppMemberExpression e

ppObjInDotRef :: Expression a -> (Expression a -> Doc) -> Doc
ppObjInDotRef i@(IntLit _ _) _ = parens (ppPrimaryExpression i)
ppObjInDotRef e p              = p e

ppArguments :: [Expression a] -> Doc
ppArguments es = 
  parens $ cat $ punctuate comma (map (ppAssignmentExpression True) es)

ppLHSExpression :: Expression a -> Doc
ppLHSExpression = ppCallExpression

-- 11.3
ppPostfixExpression :: Expression a -> Doc
ppPostfixExpression e = case e of
  UnaryAssignExpr _ PostfixInc e' -> prettyPrint e' <> text "++"
  UnaryAssignExpr _ PostfixDec e' -> prettyPrint e' <> text "--"
  _ -> ppLHSExpression e
  
-- 11.4
ppUnaryExpression :: Expression a -> Doc
ppUnaryExpression e = case e of
  PrefixExpr _ op e' -> prettyPrint op <> prefixSpace op <> ppUnaryExpression e'
  UnaryAssignExpr _ PrefixInc e' -> text "++" <> prettyPrint e'
  UnaryAssignExpr _ PrefixDec e' -> text "--" <> prettyPrint e'
  _ -> ppPostfixExpression e

prefixSpace :: PrefixOp -> Doc
prefixSpace op = case op of
  PrefixLNot   -> empty
  PrefixBNot   -> empty
  PrefixPlus   -> empty
  PrefixMinus  -> empty
  PrefixTypeof -> space
  PrefixVoid   -> space
  PrefixDelete -> space

-- 11.5
ppMultiplicativeExpression :: Expression a -> Doc
ppMultiplicativeExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpMul, OpDiv, OpMod] -> 
    ppMultiplicativeExpression e1 <+> prettyPrint op <+> ppUnaryExpression e2
  _ -> ppUnaryExpression e
  
-- 11.6
ppAdditiveExpression :: Expression a -> Doc
ppAdditiveExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpAdd, OpSub] -> 
    ppAdditiveExpression e1 <+> prettyPrint op
    <+> ppMultiplicativeExpression e2
  _ -> ppMultiplicativeExpression e

-- 11.7
ppShiftExpression :: Expression a -> Doc
ppShiftExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpLShift, OpSpRShift, OpZfRShift] -> 
    ppShiftExpression e1 <+> prettyPrint op <+> ppAdditiveExpression e2  
  _ -> ppAdditiveExpression e

-- 11.8.  
-- | @ppRelationalExpression True@ is RelationalExpression,
-- @ppRelationalExpression False@ is RelationalExpressionNoIn
ppRelationalExpression :: Bool -> Expression a -> Doc
ppRelationalExpression hasIn e = 
  let opsNoIn = [OpLT, OpGT, OpLEq, OpGEq, OpInstanceof]
      ops     = if hasIn then OpIn:opsNoIn else opsNoIn
  in case e of    
    InfixExpr _ op e1 e2 | op `elem` ops -> 
      ppRelationalExpression hasIn e1 <+> prettyPrint op
      <+> ppShiftExpression e2
    _ -> ppShiftExpression e
    
-- 11.9
ppEqualityExpression :: Bool -> Expression a -> Doc
ppEqualityExpression hasIn e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpEq, OpNEq, OpStrictEq, OpStrictNEq] ->
    ppEqualityExpression hasIn e1 <+> prettyPrint op <+> 
    ppRelationalExpression hasIn e2
  _ -> ppRelationalExpression hasIn e
  
-- 11.10
ppBitwiseANDExpression :: Bool -> Expression a -> Doc
ppBitwiseANDExpression hasIn e = case e of
  InfixExpr _ op@OpBAnd e1 e2 -> ppBitwiseANDExpression hasIn e1 <+> 
                                 prettyPrint op <+>
                                 ppEqualityExpression hasIn e2
  _ -> ppEqualityExpression hasIn e
  
ppBitwiseXORExpression :: Bool -> Expression a -> Doc
ppBitwiseXORExpression hasIn e = case e of
  InfixExpr _ op@OpBXor e1 e2 -> ppBitwiseXORExpression hasIn e1 <+>
                                 prettyPrint op <+>
                                 ppBitwiseANDExpression hasIn e2
  _ -> ppBitwiseANDExpression hasIn e
  
ppBitwiseORExpression :: Bool -> Expression a -> Doc
ppBitwiseORExpression hasIn e = case e of
  InfixExpr _ op@OpBOr e1 e2 -> ppBitwiseORExpression hasIn e1 <+>
                                prettyPrint op <+>
                                ppBitwiseXORExpression hasIn e2
  _ -> ppBitwiseXORExpression hasIn e

-- 11.11
ppLogicalANDExpression :: Bool -> Expression a -> Doc
ppLogicalANDExpression hasIn e = case e of
  InfixExpr _ op@OpLAnd e1 e2 -> ppLogicalANDExpression hasIn e1 <+>
                                 prettyPrint op <+>
                                 ppBitwiseORExpression hasIn e2
  _ -> ppBitwiseORExpression hasIn e                                 
                                 
ppLogicalORExpression :: Bool -> Expression a -> Doc
ppLogicalORExpression hasIn e = case e of
  InfixExpr _ op@OpLOr e1 e2 -> ppLogicalORExpression hasIn e1 <+>
                                prettyPrint op <+>
                                ppLogicalANDExpression hasIn e2
  _ -> ppLogicalANDExpression hasIn e
  
-- 11.12
ppConditionalExpression :: Bool -> Expression a -> Doc
ppConditionalExpression hasIn e = case e of
  CondExpr _ c et ee -> ppLogicalORExpression hasIn c <+> text "?" <+> 
                        ppAssignmentExpression hasIn et <+> colon <+>
                        ppAssignmentExpression hasIn ee
  _ -> ppLogicalORExpression hasIn e

-- 11.13
ppAssignmentExpression :: Bool -> Expression a -> Doc
ppAssignmentExpression hasIn e = case e of
  AssignExpr _ op l r -> prettyPrint l <+> prettyPrint op <+> 
                         ppAssignmentExpression hasIn r
  _ -> ppConditionalExpression hasIn e
  
-- 11.14
ppExpression :: Bool -> Expression a -> Doc
ppExpression hasIn e = case e of
  ListExpr _ es -> cat $ punctuate comma (map (ppExpression hasIn) es)
  _ -> ppAssignmentExpression hasIn e

maybe :: Maybe a -> (a -> Doc) -> Doc
maybe Nothing  _ = empty
maybe (Just a) f = f a

