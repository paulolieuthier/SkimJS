{-# LANGUAGE FlexibleInstances #-}
module Language.ECMAScript3.PrettyPrintAST where

import Language.ECMAScript3.Syntax

instance Show (JavaScript a) where
    show (Script _ stmts) = show stmts

instance Show (Id a) where
    show (Id _ str) = "Id " ++ str

instance Show (VarDecl a) where
    show (VarDecl _ name Nothing) = "VarDecl " ++ show name
    show (VarDecl _ name expr) = "VarDecl " ++ show name ++ " " ++ show expr

instance Show (Expression a) where
    show (StringLit _ value) = "StringLit " ++ value
    show (RegexpLit _ s b1 b2) = "RegexpLit " ++ show s ++ " " ++ show b1 ++ " " ++ show b2
    show (NumLit _ value) = "NumLit " ++ show value
    show (IntLit _ value) = "IntLit " ++ show value
    show (BoolLit _ value) = "BoolLit " ++ show value
    show (NullLit _) = "NullLit"
    show (ArrayLit _ exprs) = "ArrayLit " ++ show exprs
    show (ObjectLit _ items) = "ObjectLit " ++ show items
    show (ThisRef _) = "ThisRef"
    show (VarRef _ id) = "VarRef " ++ show id
    show (DotRef _ expr id) = "DotRef " ++ show expr ++ " " ++ show id
    show (BracketRef _ exprC exprK) = "BracketRef " ++ show exprC ++ " " ++ show exprK
    show (NewExpr _ exprC expr) = "NewExpr " ++ show exprC ++ " " ++ show expr
    show (PrefixExpr _ op expr) = "PrefixExpr " ++ show op ++ " " ++ show expr
    show (UnaryAssignExpr _ op lvalue) = "UnaryAssignExpr " ++ show op ++ " " ++ show lvalue
    show (InfixExpr _ op exprA exprB) = "InfixExpr " ++ show op ++ " " ++ show exprA ++ " " ++ show exprB
    show (CondExpr _ test consequent alternte) = " C "
    show (AssignExpr _ op lvalue expr) = " C "
    show (ListExpr _ exprs) = " C "
    show (CallExpr _ expr exprs) = " C "
    show (FuncExpr _ id ids stmts) = " C "

instance Show (Statement a) where 
    show (BlockStmt _ stmts) = " BLOCK "
    show (EmptyStmt _) = " EMPTY "
    show (ExprStmt _ e) = " EXPR "
    show (IfSingleStmt _ test cons) = " _ "
    show (IfStmt _ test cons alt) = " _ "
    show (SwitchStmt _ e cases) = " _ "
    show (WhileStmt _ test body) = " _ "
    show (ReturnStmt _ Nothing) = " _ "
    show (ReturnStmt _ (Just e)) = " _ "
    show (DoWhileStmt _ s e) = " _ "
    show (BreakStmt _ Nothing) = " _ "
    show (BreakStmt _ (Just label)) = " _ "
    show (ContinueStmt _ Nothing) = " _ "
    show (ContinueStmt _ (Just label)) = " _ "
    show (LabelledStmt _ label s) = " _ "
    show (ForInStmt p init e body) = " _ "
    show (ForStmt _ init incr test body) = " _ "
    show (TryStmt _ stmt mcatch mfinally) = " _ "
    show (ThrowStmt _ e) = " _ "
    show (WithStmt _ e s) = " _ "
    show (VarDeclStmt _ decls) = " VarDeclStmt " ++ show decls 
    show (FunctionStmt _ name args body) = " FunctionStmt " ++ show name ++ " " ++ show args ++ " " ++ show body ++ " "
