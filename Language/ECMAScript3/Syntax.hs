-- |ECMAScript 3 syntax. /Spec/ refers to the ECMA-262 specification,
-- 3rd edition.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.ECMAScript3.Syntax (JavaScript(..)
                                   ,unJavaScript
                                   ,Statement(..)
                                   ,isIterationStmt
                                   ,CaseClause(..)
                                   ,CatchClause(..)
                                   ,ForInit(..)
                                   ,ForInInit(..)
                                   ,VarDecl(..)
                                   ,Expression(..)
                                   ,InfixOp(..)
                                   ,AssignOp(..)
                                   ,Id(..)
                                   ,unId
                                   ,PrefixOp(..)
                                   ,Prop(..)
                                   ,UnaryAssignOp(..)
                                   ,LValue (..)
                                   ,SourcePos
                                   ,isValid
                                   ,isValidIdentifier
                                   ,isValidIdentifierName
                                   ,isReservedWord
                                   ,isValidIdStart
                                   ,isValidIdPart
                                   ,EnclosingStatement(..)
                                   ,pushLabel
                                   ,pushEnclosing
                                   ,HasLabelSet (..)
                                   ,isIter
                                   ,isIterSwitch
                                   ) where

import Text.Parsec.Pos(initialPos,SourcePos) -- used by data JavaScript
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Default.Class
import Data.Generics.Uniplate.Data
import Data.Char
import Control.Monad.State
import Control.Arrow

data JavaScript
  -- |A script in \<script\> ... \</script\> tags.
  = Script [Statement] 
  deriving (Show,Data,Typeable,Eq,Ord)

-- | extracts statements from a JavaScript type
unJavaScript :: JavaScript -> [Statement]
unJavaScript (Script stmts) = stmts

data Id = Id String 
    deriving (Show,Eq,Ord,Data,Typeable)

unId :: Id -> String
unId (Id s) = s

-- | Infix operators: see spec 11.5-11.11
data InfixOp = OpLT -- ^ @<@
             | OpLEq -- ^ @<=@
             | OpGT -- ^ @>@
             | OpGEq -- ^ @>=@
             | OpIn -- ^ @in@
             | OpInstanceof -- ^ @instanceof@
             | OpEq -- ^ @==@
             | OpNEq -- ^ @!=@
             | OpStrictEq -- ^ @===@
             | OpStrictNEq -- ^ @!===@
             | OpLAnd -- ^ @&&@
             | OpLOr -- ^ @||@
             | OpMul -- ^ @*@
             | OpDiv -- ^ @/@
             | OpMod -- ^ @%@
             | OpSub -- ^ @-@
             | OpLShift -- ^ @<<@
             | OpSpRShift -- ^ @>>@
             | OpZfRShift -- ^ @>>>@
             | OpBAnd -- ^ @&@
             | OpBXor -- ^ @^@
             | OpBOr -- ^ @|@
             | OpAdd -- ^ @+@
    deriving (Show,Data,Typeable,Eq,Ord,Enum)

-- | Assignment operators: see spec 11.13
data AssignOp = OpAssign -- ^ simple assignment, @=@
              | OpAssignAdd -- ^ @+=@
              | OpAssignSub -- ^ @-=@
              | OpAssignMul -- ^ @*=@
              | OpAssignDiv -- ^ @/=@
              | OpAssignMod -- ^ @%=@
              | OpAssignLShift -- ^ @<<=@
              | OpAssignSpRShift -- ^ @>>=@
              | OpAssignZfRShift -- ^ @>>>=@
              | OpAssignBAnd -- ^ @&=@
              | OpAssignBXor -- ^ @^=@
              | OpAssignBOr -- ^ @|=@
  deriving (Show,Data,Typeable,Eq,Ord)

-- | Unary assignment operators: see spec 11.3, 11.4.4, 11.4.5
data UnaryAssignOp = PrefixInc -- ^ @++x@
                   | PrefixDec -- ^ @--x@
                   | PostfixInc -- ^ @x++@
                   | PostfixDec -- ^ @x--@
  deriving (Show, Data, Typeable, Eq, Ord)

-- | Prefix operators: see spec 11.4 (excluding 11.4.4, 11.4.5)
data PrefixOp = PrefixLNot -- ^ @!@
              | PrefixBNot -- ^ @~@
              | PrefixPlus -- ^ @+@
              | PrefixMinus -- ^ @-@
              | PrefixTypeof -- ^ @typeof@
              | PrefixVoid -- ^ @void@
              | PrefixDelete -- ^ @delete@
  deriving (Show,Data,Typeable,Eq,Ord)

-- | Property names in an object initializer: see spec 11.1.5
data Prop = PropId Id -- ^ property name is an identifier, @foo@
            | PropString String -- ^ property name is a string, @\"foo\"@
            | PropNum Integer -- ^ property name is an integer, @42@
  deriving (Show,Data,Typeable,Eq,Ord)
 
-- | Left-hand side expressions: see spec 11.2
data LValue
  = LVar String -- ^ variable reference, @foo@
  | LDot Expression String -- ^ @foo.bar@
  | LBracket Expression Expression -- ^ @foo[bar]@
  deriving (Show, Eq, Ord, Data, Typeable) 

-- | Expressions, see spec 11
data Expression
  = StringLit String -- ^ @\"foo\"@, spec 11.1.3, 7.8
  | RegexpLit String Bool Bool 
    -- ^ @RegexpLit a regexp global?  case_insensitive?@ -- regular
    -- expression, see spec 11.1.3, 7.8
  | NumLit Double -- ^ @41.99999@, spec 11.1.3, 7.8
  | IntLit Int -- ^ @42@, spec 11.1.3, 7.8
  | BoolLit Bool -- ^ @true@, spec 11.1.3, 7.8
  | NullLit -- ^ @null@, spec 11.1.3, 7.8
  | ArrayLit [Expression] -- ^ @[1,2,3]@, spec 11.1.4
  | ObjectLit [(Prop, Expression)] -- ^ @{foo:\"bar\", baz: 42}@, spec 11.1.5
  | ThisRef -- ^ @this@, spec 11.1.1
  | VarRef Id -- ^ @foo@, spec 11.1.2
  | DotRef Expression Id -- ^ @foo.bar@, spec 11.2.1
  | BracketRef Expression {- container -} Expression {- key -} 
    -- ^ @foo[bar@, spec 11.2.1
  | NewExpr Expression {- constructor -} [Expression] 
    -- ^ @new foo(bar)@, spec 11.2.2
  | PrefixExpr PrefixOp Expression 
    -- ^ @\@e@, spec 11.4 (excluding 11.4.4, 111.4.5)
  | UnaryAssignExpr UnaryAssignOp LValue 
    -- ^ @++x@, @x--@ etc., spec 11.3, 11.4.4, 11.4.5
  | InfixExpr InfixOp Expression Expression 
    -- ^ @e1\@e2@, spec 11.5, 11.6, 11.7, 11.8, 11.9, 11.10, 11.11
  | CondExpr Expression Expression Expression
    -- ^ @e1 ? e2 : e3@, spec 11.12
  | AssignExpr AssignOp LValue Expression
    -- ^ @e1 \@=e2@, spec 11.13
  | ListExpr [Expression] -- ^ @e1, e2@, spec 11.14
  | CallExpr Expression [Expression] -- ^ @f(x,y,z)@, spec 11.2.3
  --funcexprs are optionally named
  | FuncExpr (Maybe Id) [Id] [Statement]
    -- ^ @function f (x,y,z) {...}@, spec 11.2.5, 13
  deriving (Show,Data,Typeable,Eq,Ord)

-- | Case clauses, spec 12.11
data CaseClause = CaseClause (Expression) [Statement]
                    -- ^ @case e: stmts;@
                  | CaseDefault [Statement]
                    -- ^ @default: stmts;@
  deriving (Show,Data,Typeable,Eq,Ord)

-- | Catch clause, spec 12.14
data CatchClause = CatchClause Id Statement 
                     -- ^ @catch (x) {...}@
  deriving (Show,Data,Typeable,Eq,Ord)

-- | A variable declaration, spec 12.2
data VarDecl = VarDecl Id (Maybe Expression) 
                 -- ^ @var x = e;@
  deriving (Show,Data,Typeable,Eq,Ord)
  
-- | for initializer, spec 12.6
data ForInit = NoInit -- ^ empty
               | VarInit [VarDecl] -- ^ @var x, y=42@
               | ExprInit Expression -- ^ @expr@
  deriving (Show,Data,Typeable,Eq,Ord)

-- | for..in initializer, spec 12.6
data ForInInit = ForInVar Id -- ^ @var x@
                 | ForInLVal LValue -- ^ @foo.baz@, @foo[bar]@, @z@
 deriving (Show,Data,Typeable,Eq,Ord)
  
-- | Statements, spec 12.
data Statement 
  = BlockStmt [Statement] -- ^ @{stmts}@, spec 12.1
  | EmptyStmt -- ^ @;@, spec 12.3
  | ExprStmt Expression -- ^ @expr;@, spec 12.4
  | IfStmt Expression Statement Statement 
    -- ^ @if (e) stmt@, spec 12.5
  | IfSingleStmt Expression Statement
    -- ^ @if (e) stmt1 else stmt2@, spec 12.5
  | SwitchStmt Expression [CaseClause]
    -- ^ @switch (e) clauses@, spec 12.11
  | WhileStmt Expression Statement
    -- ^ @while (e) do stmt@, spec 12.6
  | DoWhileStmt Statement Expression
    -- ^ @do stmt while (e);@, spec 12.6
  | BreakStmt (Maybe Id) -- ^ @break lab;@, spec 12.8
  | ContinueStmt (Maybe Id) -- ^ @continue lab;@, spec 12.7
  | LabelledStmt Id Statement -- ^ @lab: stmt@, spec 12.12
  | ForInStmt ForInInit Expression Statement 
    -- ^ @for (x in o) stmt@, spec 12.6
  | ForStmt ForInit        
            (Maybe Expression) -- test
            (Maybe Expression) -- increment
            Statement          -- body 
    -- ^ @ForStmt a init test increment body@, @for (init; test,
    -- increment) body@, spec 12.6
  | TryStmt Statement {-body-} (Maybe CatchClause)
      (Maybe Statement) {-finally-}
    -- ^ @try stmt catch(x) stmt finally stmt@, spec 12.14
  | ThrowStmt Expression
    -- ^ @throw expr;@, spec 12.13
  | ReturnStmt (Maybe Expression)
    -- ^ @return expr;@, spec 12.9
  | WithStmt Expression Statement
    -- ^ @with (o) stmt@, spec 12.10
  | VarDeclStmt [VarDecl]
    -- ^ @var x, y=42;@, spec 12.2
  | FunctionStmt Id {-name-} [Id] {-args-} [Statement] {-body-}
    -- ^ @function f(x, y, z) {...}@, spec 13
  deriving (Show,Data,Typeable,Eq,Ord)  

-- | Returns 'True' if the statement is an /IterationStatement/
-- according to spec 12.6.
isIterationStmt :: Statement -> Bool
isIterationStmt s = case s of
  WhileStmt {} -> True
  DoWhileStmt {} -> True
  ForStmt {} -> True
  ForInStmt {} -> True
  _ -> False
  
-- | The ECMAScript standard defines certain syntactic restrictions on
-- programs (or, more precisely, statements) that aren't easily
-- enforced in the AST datatype. These restrictions have to do with
-- labeled statements and break/continue statement, as well as
-- identifier names. Thus, it is possible to manually generate AST's
-- that correspond to syntactically incorrect programs. Use this
-- predicate to check if an 'JavaScript' AST corresponds to a
-- syntactically correct ECMAScript program.
isValid :: JavaScript -> Bool
-- =From ECMA-262-3=
-- A program is considered syntactically incorrect if either of the
-- following is true:
-- * The program contains a continue statement without the optional
-- Identifier, which is not nested, directly or indirectly (but not
-- crossing function boundaries), within an IterationStatement.
-- * The program contains a continue statement with the optional
-- Identifier, where Identifier does not appear in the label set of an
-- enclosing (but not crossing function boundaries) IterationStatement.
-- * The program contains a break statement without the optional
-- Identifier, which is not nested, directly or indirectly (but not
-- crossing function boundaries), within an IterationStatement or a
-- SwitchStatement.
-- * The program contains a break statement with the optional
-- Identifier, where Identifier does not appear in the label set of an
-- enclosing (but not crossing function boundaries) Statement.
-- * The program contains a LabelledStatement that is enclosed by a
-- LabelledStatement with the same Identifier as label. This does not
-- apply to labels appearing within the body of a FunctionDeclaration
-- that is nested, directly or indirectly, within a labelled
-- statement.
-- * The identifiers should be valid. See spec 7.6
isValid js = checkIdentifiers js && checkBreakContinueLabels js
  where checkIdentifiers :: JavaScript -> Bool
        checkIdentifiers js =
          and $ map isValidIdentifierName $
          [n | (Id n) :: Id <- universeBi js] ++
          [n | (LVar n) :: LValue <- universeBi js] ++
          [n | (LDot _ n) :: LValue <- universeBi js]
        checkBreakContinueLabels js@(Script body) = and $ map checkStmt $
           body ++ concat ([body | FuncExpr _ _ body <- universeBi js] ++
                           [body | FunctionStmt _ _ body <- universeBi js])

checkStmt :: Statement -> Bool
checkStmt s = evalState (checkStmtM s) ([], [])

checkStmtM :: Statement -> State ([Label], [EnclosingStatement]) Bool
checkStmtM stmt = case stmt of
  ContinueStmt mlab -> do
    encls <- gets snd
    let enIts = filter isIter encls
    return $ case mlab of
      Nothing  -> not $ null enIts
      Just lab -> any (elem (unId lab) . getLabelSet) enIts
  BreakStmt mlab -> do
    encls <- gets snd
    return $ case mlab of
      Nothing  -> any isIterSwitch encls
      Just lab -> any (elem (unId lab) . getLabelSet) encls
  LabelledStmt lab s -> do
    labs <- gets fst
    if (unId lab) `elem` labs then return False
      else pushLabel lab $ checkStmtM s
  WhileStmt _ s   -> iterCommon s
  DoWhileStmt s _ -> iterCommon s
  ForStmt _ _ _ s -> iterCommon s
  ForInStmt _ _ s -> iterCommon s
  SwitchStmt _ cs -> pushEnclosing EnclosingSwitch $ liftM and $ mapM checkCaseM cs
  BlockStmt ss -> pushEnclosing EnclosingOther $ liftM and $ mapM checkStmtM ss
  IfStmt _ t e -> liftM2 (&&) (checkStmtM t) (checkStmtM e)
  IfSingleStmt _ t -> checkStmtM t
  TryStmt body mcatch mfinally -> liftM2 (&&) (checkStmtM body) $
    liftM2 (&&) (maybe (return True) checkCatchM mcatch)
                (maybe (return True) checkStmtM mfinally)
  WithStmt _ body -> checkStmtM body
  _ -> return True

iterCommon s = pushEnclosing EnclosingIter $ checkStmtM s

pushEnclosing :: Monad m => ([Label] -> EnclosingStatement)
              -> StateT ([Label], [EnclosingStatement]) m a
              -> StateT ([Label], [EnclosingStatement]) m a
pushEnclosing ctor = bracketState (\(labs, encls) -> ([], ctor labs:encls))

pushLabel :: Monad m => Id -> StateT ([Label], [EnclosingStatement]) m a
          -> StateT ([Label], [EnclosingStatement]) m a
pushLabel l = bracketState (first (unId l:))

checkCaseM c = let ss = case c of
                     CaseClause _ body -> body
                     CaseDefault body -> body
               in liftM and $ mapM checkStmtM ss

checkCatchM (CatchClause _ body) = checkStmtM body

bracketState :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
bracketState f m = do original <- get
                      modify f
                      rv <- m
                      put original
                      return rv

-- | Checks if an identifier name is valid according to the spec
isValidIdentifier :: Id -> Bool
isValidIdentifier (Id name) = isValidIdentifierName name

-- | Checks if the 'String' represents a valid identifier name
isValidIdentifierName :: String -> Bool
isValidIdentifierName name = case name of
  "" -> False
  (c:cs) -> isValidIdStart c && and (map isValidIdPart cs) && (not $ isReservedWord name)

-- | Checks if a string is in the list of reserved ECMAScript words
isReservedWord :: String -> Bool
isReservedWord = (`elem` reservedWords)
  where reservedWords = keyword ++ futureReservedWord ++ nullKw ++ boolLit
        keyword = ["break", "case", "catch", "continue", "default", "delete"
                  ,"do", "else", "finally", "for", "function", "if", "in"
                  ,"instanceof", "new", "return", "switch", "this", "throw"
                  ,"try", "typeof", "var", "void", "while", "with"]
        futureReservedWord = ["abstract", "boolean", "byte", "char", "class"
                             ,"const", "debugger", "enum", "export", "extends"
                             ,"final", "float", "goto", "implements", "int"
                             ,"interface", "long", "native", "package", "private"
                             ,"protected", "short", "static", "super"
                             ,"synchronized", "throws", "transient", "volatile"]
        nullKw = ["null"]
        boolLit = ["true", "false"]

-- | Checks if a character is valid at the start of an identifier
isValidIdStart :: Char -> Bool
isValidIdStart c = unicodeLetter c || c == '$' || c == '_'
  where unicodeLetter c = case generalCategory c of
          UppercaseLetter -> True
          LowercaseLetter -> True
          TitlecaseLetter -> True
          ModifierLetter  -> True
          OtherLetter     -> True
          LetterNumber    -> True
          _               -> False

-- | Checks if a character is valid in an identifier part
isValidIdPart :: Char -> Bool
isValidIdPart c = isValidIdStart c || isValidIdPartUnicode c
  where isValidIdPartUnicode c = case generalCategory c of
          NonSpacingMark       -> True
          SpacingCombiningMark -> True
          DecimalNumber        -> True
          ConnectorPunctuation -> True
          _                    -> False
        
          
data EnclosingStatement = EnclosingIter [Label]
                          -- ^ The enclosing statement is an iteration statement
                        | EnclosingSwitch [Label]
                          -- ^ The enclosing statement is a switch statement
                        | EnclosingOther [Label]
                          -- ^ The enclosing statement is some other
                          -- statement.  Note, `EnclosingOther` is
                          -- never pushed if the current `labelSet` is
                          -- empty, so the list of labels in this
                          -- constructor should always be non-empty

instance Show EnclosingStatement where
  show (EnclosingIter ls) = "iteration" ++ show ls
  show (EnclosingSwitch ls) = "switch" ++ show ls
  show (EnclosingOther ls) = "statement" ++ show ls

isIter :: EnclosingStatement -> Bool
isIter (EnclosingIter _) = True
isIter _                 = False

isIterSwitch :: EnclosingStatement -> Bool
isIterSwitch (EnclosingIter _)   = True
isIterSwitch (EnclosingSwitch _) = True
isIterSwitch _                   = False

class HasLabelSet a where
  getLabelSet :: a -> [Label]
  setLabelSet :: [Label] -> a -> a

modifyLabelSet :: HasLabelSet a => ([Label] -> [Label]) -> a -> a
modifyLabelSet f a = setLabelSet (f $ getLabelSet a) a

instance HasLabelSet EnclosingStatement where
  getLabelSet e = case e of
    EnclosingIter ls   -> ls
    EnclosingSwitch ls -> ls
    EnclosingOther ls  -> ls
  setLabelSet ls e = case e of
    EnclosingIter _   -> EnclosingIter ls
    EnclosingSwitch _ -> EnclosingSwitch ls
    EnclosingOther _  -> EnclosingOther ls

type Label = String
