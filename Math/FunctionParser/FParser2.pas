unit FParser2;

{ Modified for TP7, 2004 Romtek

  From FuncParser.pas (Delphi)
  (FVars) 2003 Ilya FResults. Scheglov, student of the Department of Natural Sciences,
  Moscow State Technical University named after N.E. Bauman
  Bauman Moscow State Technical University, Moscow }

interface

uses
  Classes, Math, Windows;

type
  TOperation = { opcode }
    (
      _Unknown,
      _List, // (...)

      _Const,
      _Variable,
      _Assign,
      _EoC,

      _Sum,
      _Substract,
      _Multiply,
      _Divide,
      _Sqr,
      _Sqrt,
      _Power,
      _Sin,
      _Cos,
      _Tan,
      _Cotan,
      _Exp,
      _Ln,
      _Unary, { - }

      _Mod,
      _Trunc,
      _Round,
      _Arcsin,
      _Arccos,
      _Arctan,
      _ArcCot,
      _Sinh,
      _Cosh,
      _Tanh,
      _Coth,

      _Length,
      _Eval,
      _Equal,
      _Unequal,
      _Less,
      _More,
      _LessEqual,
      _MoreEqual,
      _Or,
      _And,
      _Xor,
      _Not,

      _Count,
      _Avg,
      _Max,
      _Min,

      _Other
    );

  TOperand = Object
  private
    FOperation: TOperation;
    FFunctionIndex: integer;
    FParams: array of integer;
    FSnippet: string;
    FPos: TPoint;
    FResult: Variant;
    function GetParams(aIndex: integer): Integer;
  public
    procedure Add(aValue: integer);

    function Count: integer;
    function First: integer;
    function Second: integer;
    property Params[index: integer]: integer read GetParams;

    property Operation: TOperation read FOperation;
  end;
  POperand = ^TOperand;

  TAoTOperand = Array of TOperand;
  TAoReal = Array of Variant; { values }

  TParsedFunction = class;

  TFunctionCall = function (Sender: TParsedFunction; Operant: TOperand): Variant of object;

  TParsedFunction = class
  private
    FResults: TAoReal; { elementary blocks }

    FOperands: TAoTOperand; { numbers of el. blocks, envolved in operation }
    FBlockLen: integer; { free index, also length of array FOperands }

    FVars: TAoReal;

    FVarNames: TStringList;
    FFunctions: TStringList;
    FBuildInFunctions: integer;

    FError: string;
    FErrorCode, FErrorLen: integer;
    FErrorPos: TPoint;
    FErrorOperand: integer;

    function GetVarCount: integer;
    function GetVarName(index: integer): string;
    function GetVars(index: integer): Variant;
    function GetOutcome(index: integer): Variant; { last index for const, starting from 3 }

    function AddOperand(aOperator: TOperation = _Unknown): integer;
    function AddVar(aValue: variant): integer;
    procedure SaveOperation(aOperator: TOperation; aFunctionIndex: integer = -1);
    function GetErrorPos: TPoint;
  protected
    property BlockIdx: integer read FBlockLen;
    procedure SetError(aCode: integer; aMessage: string; aStart, aLen: integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    constructor Clone(aSource: TParsedFunction); virtual;

    function Compile(s: String): boolean;

    function Execute: boolean;

    function DeclareVar(aName: string): integer;
    procedure SetVar(aName: string; aValue: Variant);
    procedure DeclareFunction(aName: string; aCallback: TFunctionCall);

    function VarIndexByName(aName: string): integer;
    property Vars[index: integer]: Variant read GetVars;
    property VarNames[index: integer]: string read GetVarName;
    property VarCount: integer read GetVarCount;

    property Outcome[index: integer]: Variant read GetOutcome;

    property Error: string read FError;
    property ErrorCode: integer read FErrorCode;
    property ErrorPos: TPoint read GetErrorPos;
    property ErrorLen: integer read FErrorLen;
  End;

function VarListCount(const v: Variant): integer;
function VarIsList(const v: Variant): boolean;

implementation

uses
  SysUtils, Variants;

function VarIsList(const v: Variant): boolean;
begin
  Result :=
    VarIsArray(v) and
    (VarArrayDimCount(v) = 1);
end;

function VarListCount(const v: Variant): integer;
begin
  Result := 0;
  if VarIsList(v) then
    Result := VarArrayHighBound(v, 1) - VarArrayLowBound(v, 1) + 1;
end;

type
  TFunctionCallObj = class
  private
    FCallBack: TFunctionCall;
  public
    constructor Create(aCallBack: TFunctionCall);
    property CallBack: TFunctionCall read FCallBack write FCallBack;
  end;

function TParsedFunction.AddOperand(aOperator: TOperation = _Unknown): integer;
begin
  Inc(FBlockLen);
  SetLength(FOperands, FBlockLen+1);
  FOperands[FBlockLen].FOperation := aOperator;
  FErrorOperand := FBlockLen;
  SetLength(FResults, FBlockLen+1); { allow block results }
  Result := FBlockLen;
end;

constructor TParsedFunction.Create;
begin
  FVarNames := TStringList.Create;
  FFunctions := TStringList.Create;

  for var i := Low(TOperation) to High(TOperation) do
    case i of
      _Unknown:      FFunctions.Add('');
      _List:      FFunctions.Add('');
      _Const:     FFunctions.Add('');
      _Variable:  FFunctions.Add('');
      _Assign:    FFunctions.Add(':=');
      _EoC:       FFunctions.Add(';');
      _Sum:       FFunctions.Add('+');
      _Substract: FFunctions.Add('-');
      _Multiply:  FFunctions.Add('*');
      _Divide:    FFunctions.Add('/');
      _Sqr:       FFunctions.Add('SQR');
      _Sqrt:      FFunctions.Add('SQRT');
      _Power:     FFunctions.Add('^');
      _Sin:       FFunctions.Add('SIN');
      _Cos:       FFunctions.Add('COS');
      _Tan:       FFunctions.Add('TAN');
      _Cotan:     FFunctions.Add('COT');
      _Exp:       FFunctions.Add('EXP');
      _Ln:        FFunctions.Add('LN');
      _Unary:     FFunctions.Add('');
      _Mod:       FFunctions.Add('%');
      _Trunc:     FFunctions.Add('TRUNC');
      _Round:     FFunctions.Add('ROUND');
      _Arcsin:    FFunctions.Add('ASIN');
      _Arccos:    FFunctions.Add('ACOS');
      _Arctan:    FFunctions.Add('ATAN');
      _ArcCot:    FFunctions.Add('ACOT');
      _Sinh:      FFunctions.Add('SINH');
      _Cosh:      FFunctions.Add('COSH');
      _Tanh:      FFunctions.Add('TANH');
      _Coth:      FFunctions.Add('COTH');

      // text
      _Length:    FFunctions.Add('LEN');
      _Eval:      FFunctions.Add('EVAL');
      _Equal:     FFunctions.Add('=');
      _Unequal:   FFunctions.Add('<>');
      _Less:      FFunctions.Add('<');
      _More:      FFunctions.Add('>');
      _LessEqual: FFunctions.Add('<=');
      _MoreEqual: FFunctions.Add('>=');
      _Or:        FFunctions.Add('OR');
      _And:       FFunctions.Add('AND');
      _Xor:       FFunctions.Add('XOR');
      _Not:       FFunctions.Add('NOT');

      _Count:     FFunctions.Add('COUNT');
      _Avg:       FFunctions.Add('AVG');
      _Max:       FFunctions.Add('MAX');
      _Min:       FFunctions.Add('MIN');
    else
      {_Other: }
    end;

  FBuildInFunctions := FFunctions.Count;
end;

destructor TParsedFunction.Destroy;
var
  FOperations: TFunctionCallObj;
begin
  while FFunctions.Count > 0 do
  begin
    FOperations := TFunctionCallObj(FFunctions.Objects[0]);
    FreeAndNil(FOperations);
    FFunctions.Delete(0);
  end;
  FreeAndNil(FFunctions);
  FreeAndNil(FVarNames);
  inherited;
end;

function TParsedFunction.GetErrorPos: TPoint;
begin
  if FErrorOperand >= 0 then
    Result := FOperands[FErrorOperand].FPos
  else
    Result := FErrorPos;
end;

function TParsedFunction.GetOutcome(index: integer): Variant;
begin
  Result := FResults[index];
end;

function TParsedFunction.GetVarCount: integer;
begin
  Result := FVarNames.Count;
end;

function TParsedFunction.GetVarName(index: integer): string;
begin
  Result := FVarNames[index-1];
end;

function TParsedFunction.GetVars(index: integer): Variant;
begin
  Result := FVars[index];
end;

procedure TParsedFunction.DeclareFunction(aName: string;
  aCallback: TFunctionCall);
begin
  aName := UpperCase(aName);
  if FFunctions.IndexOf(aName) < 0 then
    FFunctions.AddObject(aName, TFunctionCallObj.Create(aCallback));
end;

function TParsedFunction.DeclareVar(aName: string): integer;
begin
  SetVar(aName, 0);
  Result := VarIndexByName(aName);
end;

Function TParsedFunction.Execute: boolean;

  function ExecCommand(commandStart, commandEnd: integer): boolean;
  var
    i: Integer;
    Callback: ^TFunctionCall;
    op, op1, op2: POperand;
  begin
    Result := False;

    for i := commandEnd downto commandStart do
    try
      op := @FOperands[i];
      FErrorOperand := i;
      case op.Operation of
        _Unknown,
        _List:
          Continue;

        _EoC:
          begin
            Inc(FErrorPos.Y);
            Continue; { operations which are added to the grammar which only represent (..) }
          end;

        _Const:
          begin
            FResults[i] := op.FResult;
            Continue;
          end;
        _Variable:
          FResults[i] := FVars[op.First]; { Assignment }
        _Assign:
          begin
            Assert(FOperands[op.First].FOperation = _Variable, 'Must assign to variable');
            FOperands[op.First].FResult := FResults[op.Second];
            FVars[FOperands[op.First].First] := FResults[op.Second]; { Assignment }
            Continue;
          end;
        _Sum:
          if VarIsList(FResults[i+1]) then
          begin
            var cnt := VarListCount(FResults[i+1]);
            FResults[i] := FResults[i+1][VarArrayLowBound(FResults[i+1], 1)];
            for var a := VarArrayLowBound(FResults[i+1], 1)+1 to VarArrayHighBound(FResults[i+1], 1) do
              FResults[i] := FResults[i] + FResults[i+1][a];
          end
          else
          begin
            var v1 := VarType(FResults[op.First]);
            var v2 := VarType(FResults[op.Second]);
            if (v1 = varString) or (v1 = varUString) or
               (v2 = varString) or (v2 = varUString) then
               FResults[i] := VarToStr(FResults[op.First])+VarToStr(FResults[op.Second])
            else
            begin
              FResults[i] := FResults[op.First];
              for var p:=1 to op.Count-1 do
                FResults[i] := FResults[i] + FResults[op.Params[p]]; { Summ }
            end;
          end;
        _Substract:
          FResults[i] := FResults[op.First] - FResults[op.Second]; { Substract }
        _Multiply:
          FResults[i] := FResults[op.First] * FResults[op.Second]; { Multiplication }
        _Divide:
          FResults[i] := FResults[op.First] / FResults[op.Second]; { Division }
        _Sqr:
          FResults[i] := sqr(FResults[op.First]); { ^2 }
        _Sqrt:
          FResults[i] := sqrt(FResults[op.First]); { square root }
        _Power:
          FResults[i] := Power(FResults[op.First], FResults[op.Second]); { Power }
        _Sin:
          FResults[i] := sin(FResults[op.First]); { Sin }
        _Cos:
          FResults[i] := cos(FResults[op.First]); { Cos }
        _Tan:
          FResults[i] := tan(FResults[op.First]); { Tangence }
        _Cotan:
          FResults[i] := Cotan(FResults[op.First]); { Cotangence }
        _Exp:
          FResults[i] := exp(FResults[op.First]); { exp }
        _Ln:
          FResults[i] := ln(FResults[op.First]); { ln }

        _Unary:
          FResults[i] := -FResults[op.First]; { unary - }

        _Mod:
          FResults[i] := (FResults[op.First] mod FResults[op.Second]); { whole part }
        _Trunc:
          FResults[i] := trunc(FResults[op.First]); { whole part }
        _Round:
          FResults[i] := round(FResults[op.First]); { round }
        _Arcsin:
          FResults[i] := arcsin(FResults[op.First]); { arcsin }
        _Arccos:
          FResults[i] := arccos(FResults[op.First]); { arccos }
        _Arctan:
          FResults[i] := arctan(FResults[op.First]); { arctan }
        _ArcCot:
          FResults[i] := ArcCot(FResults[op.First]); { arccotan }
        _Sinh:
          FResults[i] := Sinh(FResults[op.First]); { hyp sin }
        _Cosh:
          FResults[i] := Cosh(FResults[op.First]); { hyp cos }
        _Tanh:
          FResults[i] := Tanh(FResults[op.First]); { hyp tan }
        _Coth:
          FResults[i] := coth(FResults[op.First]); { hyp cotan }
        _Length:
          FResults[i] := Length(FResults[op.First]);
        _Eval:
          begin
            var pf := TParsedFunction.Clone(Self);
            try
              pf.Compile(FResults[op.First]);
              if pf.Execute then
                FResults[i] := pf.Outcome[0]
              else
              begin
                SetError(pf.ErrorCode, pf.Error, 0,0);
                raise Exception.Create('Evaluation error');
              end;
            finally
              FreeAndNil(pf);
            end;
          end;
        _Equal:
          FResults[i] := (FResults[op.First] = FResults[op.Second]);
        _UnEqual:
          FResults[i] := (FResults[op.First] <> FResults[op.Second]);
        _Less:
          FResults[i] := (FResults[op.First] < FResults[op.Second]);
        _LessEqual:
          FResults[i] := (FResults[op.First] <= FResults[op.Second]);
        _MoreEqual:
          FResults[i] := (FResults[op.First] >= FResults[op.Second]);
        _More:
          FResults[i] := (FResults[op.First] > FResults[op.Second]);

        _Or:
          begin
            var args := FOperands[op.First];
            if (args.Operation = _List) then
              FResults[i] := (FResults[args.First] or FResults[args.Second])
            else
              FResults[i] := args.FResult;
          end;
        _And:
          begin
            var args := FOperands[op.First];
            if (args.Operation = _List) then
              FResults[i] := (FResults[args.First] and FResults[args.Second])
            else
              FResults[i] := args.FResult;
          end;
        _Xor:
          begin
            var args := FOperands[op.First];
            if (args.Operation = _List) then
              FResults[i] := (FResults[args.First] xor FResults[args.Second])
            else
              FResults[i] := args.FResult;
          end;

        _Not:
          begin
            var args := FOperands[op.First];
            FResults[i] := not args.FResult;
          end;

        _Count:
          if VarIsList(FResults[i+1]) then
            FResults[i] := VarListCount(FResults[i+1])
          else
          if (FOperands[i+1].FOperation = _List) then
            FResults[i] := FOperands[i+1].Count
          else
            FResults[i] := FOperands[i].Count;
        _Avg:
          if VarIsList(FResults[i+1]) then
          begin
            var cnt := VarListCount(FResults[i+1]);
            FResults[i] := 0;
            for var a := VarArrayLowBound(FResults[i+1], 1) to VarArrayHighBound(FResults[i+1], 1) do
              FResults[i] := FResults[i] + FResults[i+1][a] / cnt;
          end
          else
          if FOperands[i+1].FOperation = _List then
          begin
            FResults[i] := 0;
            for var a := 0 to FOperands[i+1].Count-1 do
              FResults[i] := FResults[i] +
                FResults[FOperands[i+1].Params[a]] / FOperands[i+1].Count;
          end
          else
            FResults[i] := FOperands[i+1].FResult;
        _Max:
          if VarIsList(FResults[i+1]) then
          begin
            var cnt := VarListCount(FResults[i+1]);
            FResults[i] := FResults[i+1][VarArrayLowBound(FResults[i+1], 1)];
            for var a := VarArrayLowBound(FResults[i+1], 1)+1 to VarArrayHighBound(FResults[i+1], 1) do
              if FResults[i] < FResults[i+1][a] then
                FResults[i] := FResults[i+1][a];
          end
          else
          if (FOperands[i+1].FOperation = _List) and
             (FOperands[i+1].Count > 0) then
          begin
            FResults[i] := FResults[FOperands[i+1].First];
            for var a := 1 to FOperands[i+1].Count-1 do
              if FResults[i] < FResults[FOperands[i+1].Params[a]] then
                FResults[i] := FResults[FOperands[i+1].Params[a]]
          end
          else
            FResults[i] := FOperands[i+1].FResult;
        _Min:
          if VarIsList(FResults[i+1]) then
          begin
            var cnt := VarListCount(FResults[i+1]);
            FResults[i] := FResults[i+1][VarArrayLowBound(FResults[i+1], 1)];
            for var a := VarArrayLowBound(FResults[i+1], 1)+1 to VarArrayHighBound(FResults[i+1], 1) do
              if FResults[i] > FResults[i+1][a] then
                FResults[i] := FResults[i+1][a];
          end
          else
          if (FOperands[i+1].FOperation = _List) and
             (FOperands[i+1].Count > 0) then
          begin
            FResults[i] := FResults[FOperands[i+1].First];
            for var a := 1 to op.Count-1 do
              if FResults[i] > FResults[FOperands[i+1].Params[a]] then
                FResults[i] := FResults[FOperands[i+1].Params[a]]
          end
          else
            FResults[i] := FOperands[i+1].FResult;
      else
        begin
          if (op.FFunctionIndex >= 0) then
          begin
            if (FOperands[op.First].FOperation = _List) then
              FResults[i] := TFunctionCallObj(
                FFunctions.Objects[op.FFunctionIndex])
                .CallBack(Self, FOperands[op.First])
            else
              FResults[i] := TFunctionCallObj(
                FFunctions.Objects[op.FFunctionIndex])
                .CallBack(Self, FOperands[i]);

            if VarIsArray(FResults[i]) then
            begin
              // split elements into FVars and Params indexes with a list
            end;
          end
          else
            raise Exception.Create('Invalid operation index: '+IntToStr(op.FFunctionIndex));
        end;
      end;

      op.FResult := FResults[i]; // also store a local copy for debugging
    except
      on E: Exception do
        raise Exception.Create(
          'Block['+IntToStr(i)+']: "'+
          FFunctions[Ord(op.Operation)]+'", "'+
          op.FSnippet+'" '+
          E.Message);
    end;
    Result := True;
  end; { proc }

begin
  { from StartBlock find the next _EoC. if there is none found, we simply use the last block.
    we can stop the cycles once StartBlock reached the end of all blocks }
  var startBlock := 0;
  FErrorPos.X := 0;
  FErrorPos.Y := 0;
  while (startBlock < FBlockLen) do
  begin
    for var b := startBlock to FBlockLen do
      if (FOperands[b].FOperation  = _EoC) or (b = FBlockLen) then
      begin
        Result := ExecCommand(startBlock, b);
        startBlock := b+1;
        Break;
      end;
  end;
end;

function TParsedFunction.AddVar(aValue: Variant): integer;
begin
  Result := Length(FVars);
  SetLength(FVars, Result+1);
  FVars[Result] := avalue;
end;

constructor TParsedFunction.Clone(aSource: TParsedFunction);
begin
  Create;

  FFunctions.Assign( aSource.FFunctions );
  FBuildInFunctions := aSource.FBuildInFunctions;
end;

function TParsedFunction.Compile(s: String): boolean;
const
  letter: set of Char = ['a' .. 'z', 'A' .. 'Z', '_'];
  digit: set of Char = ['0' .. '9'];
  hex: set of Char = ['0' .. '9','$','A'..'F','a'..'f'];
  operand: set of Char = ['-', '+', '*','=', '/', '^', '%'];
  { bracket  : set of Char = ['(',')']; }
  { variable : set of Char = ['x','y','z']; }

var

  i, j, n: integer; { counters }
  len: integer;
  ls: string;

  { searches ch in s OUTSIDE brackets (and not in a string) in given interval }
  function MyPos(const ch: string; const start, fin: Integer): Integer;
  var
    i, br, len: integer;
    str: boolean;
    q: string;
  begin
    MyPos := 0;
    br := 0;
    len := Length(ch);
    str := False;
    For i := fin downto start do
    begin
      case s[i] of
        '"':
          str := not str;
        '(':
          if not str then inc(br);
        ')':
          if not str then dec(br);
      end;
      if (not str) and (br = 0) and (ch = Copy(s, i, len)) then
        MyPos := i;
    end;

  end;

  procedure ReversePluses(const start, fin: Integer);
  var
    i, br: integer;
    ch: Char;
  begin
    br := 0;
    for i := start to fin do
    begin
      case s[i] of
        '(':
          inc(br);
        ')':
          dec(br);
      end;
      if br = 0 then
      begin
        ch := s[i];
        if s[i] = '+' then
          ch := '-';
        if s[i] = '-' then
          ch := '+';
        s[i] := ch;
      end;
    end;
  end;

  procedure ReverseDiv(const start, fin: Integer);
  var
    i, br: integer;
    ch: Char;
  begin
    br := 0;
    for i := start to fin do
    begin
      case s[i] of
        '(':
          inc(br);
        ')':
          dec(br);
      end;
      if br = 0 then
      begin
        ch := s[i];
        if s[i] = '/' then
          ch := '*';
        if s[i] = '*' then
          ch := '/';
        s[i] := ch;
      end;
    end;
  end;

  function ReadString(const start: Integer): string;
  var
    cp: Integer;
    strnum: string;
    quote: string;
  begin
    Result := '';
    quote := s[start]; // either " or '
    cp := start+1;
    repeat
      inc(cp);
      if Copy(s, cp, 2) = '\'+quote then Inc(cp,2); // skip \" as one char
    until (cp > Length(s)) or (s[cp] = quote);
    Result := StringReplace(Copy(s, start+1, cp - start-1),'\'+quote,quote,[rfReplaceAll]);
  end;

  procedure ReadHex(const start: Integer; var num: Real);
  var
    cp: Integer;
    strnum: string;
  begin
    cp := start;
    repeat
      inc(cp);
    until (cp > Length(s)) or not((s[cp] in hex));
    strnum := Copy(s, start, cp - start);
    num := StrToInt(strnum);
  end;

  procedure ReadNumber(const start: Integer; var num: Real);
  var
    cp: Integer;
    strnum: string;
    errorcode: integer;
  begin
    cp := start;
    repeat
      inc(cp);
    until (cp > Length(s)) or not((s[cp] in digit) or (s[cp] = '.'));
    strnum := Copy(s, start, cp - start);
    val(strnum, num, errorcode);
  end;

  { index of FResults block is BlockIdx }
  procedure ParseExpr(start, fin, BlockIdx: Integer);
  var
    cp: Integer; { cur position }
    ss, st: string;
    mynum: Real;
    i, br: Integer;
    br_ok: boolean;

    procedure ParseLeft;
    begin
      AddOperand;
      FOperands[BlockIdx].Add(FBlockLen);
      FOperands[FBlockLen].FSnippet := Copy(s, start, cp-start);
      FOperands[FBlockLen].FPos := FErrorPos;
      ParseExpr(start, cp - 1, FBlockLen);
    end;
    procedure ParseRight;
    begin
      AddOperand;
      FOperands[BlockIdx].Add(FBlockLen);
      FOperands[FBlockLen].FSnippet := Copy(s, cp+1, fin-cp);
      FOperands[FBlockLen].FPos := FErrorPos;
      ParseExpr(cp + 1, fin, FBlockLen);
    end;
    procedure Repproc3;
    begin
      AddOperand;
      FOperands[BlockIdx].Add(FBlockLen); { reference to _List }
      FOperands[FBlockLen].FSnippet := Copy(s, cp, fin-cp+1);
      FOperands[FBlockLen].FPos := FErrorPos;
      FOperands[BlockIdx+1].FOperation := _List;
      ParseExpr(cp, fin, FBlockLen);
    end;

  begin

    repeat
      FErrorPos.X := start-1;
      FErrorPos.Y := 0;
      FErrorLen := fin - start + 1;

      ss := Trim(Copy(s, start, fin - start + 1)); { local snippet }
      if ss = '' then Break;

      { skip the white spaces }
      while (s[start] = ' ') and (start < fin) do inc(start);
      while (s[fin] = ' ') and (start < fin) do dec(fin);

      FOperands[FBlockLen].FSnippet := Copy(s,start,fin-start+1);
      FErrorPos.X := start;
      FOperands[FBlockLen].FPos := FErrorPos;

      { first get rid of useless enclosing brackets if present }
      { like here: (sin(x)/cos(y)) }

      If (s[start] = '(') and (s[fin] = ')') then
      begin

        { If we have any operator within brackets at which }
        { bracket counter (br) = 0, then we MUST NOT remove brackets }
        { If there is none, we CAN do that. }

        br_ok := true; { we CAN remove by default }
        br := 0;
        for i := start to fin do
          Case s[i] of
            '(':
              inc(br);
            ')':
              dec(br);
            '+', '-', '*', '^', '/', '%':
              if br = 0 then
                br_ok := false;
          end;

        if br_ok then
        begin
          inc(start);
          dec(fin);
          continue;
        end;

      end;

      { seek for End of Command }
      cp := MyPos(';', start, fin);
      if cp > 0 then
      begin
        ParseExpr(start, cp - 1, FBlockLen);
        Inc(cp);
        Inc(FErrorPos.Y);

        if (cp < fin) then
        begin
          AddOperand(_EoC);

          AddOperand;
          ParseExpr(cp, fin, FBlockLen);
        end;

        break;
      end;

      { seek for , }
      cp := MyPos(',', start, fin);
      If cp > 0 then
      begin
        while (cp > start) do
        begin
          ParseLeft;
          start := cp+1;
          cp := MyPos(',', start, fin);
        end;
        cp := fin+1;
        ParseLeft;
        break;
      end;

      { seek for <= }
      cp := MyPos('<=', start, fin);
      if cp > 0 then
      begin
        SaveOperation(_LessEqual);
        ParseLeft;
        Inc(cp);
        ParseRight;
        break;
      end;

      { seek for >= }
      cp := MyPos('>=', start, fin);
      if cp > 0 then
      begin
        SaveOperation(_MoreEqual);
        ParseLeft;
        Inc(cp);
        ParseRight;
        break;
      end;

      { seek for <> }
      cp := MyPos('<>', start, fin);
      if cp > 0 then
      begin
        SaveOperation(_UnEqual);
        ParseLeft;
        Inc(cp);
        ParseRight;
        break;
      end;

      { seek for := }
      cp := MyPos(':=', start, fin);
      if cp > 0 then
      begin
        SaveOperation(_Assign);
        ParseLeft;
        Inc(cp);
        ParseRight;
        break;
      end;

      { seek for = }
      cp := MyPos('=', start, fin);
      if cp > 0 then
      begin
        SaveOperation(_Equal);
        ParseLeft;
        ParseRight;
        break;
      end;

      { seek for < }
      cp := MyPos('<', start, fin);
      if cp > 0 then
      begin
        SaveOperation(_Less);
        ParseLeft;
        ParseRight;
        break;
      end;

      { seek for = }
      cp := MyPos('>', start, fin);
      if cp > 0 then
      begin
        SaveOperation(_More);
        ParseLeft;
        ParseRight;
        break;
      end;

      { seek for + }
      cp := MyPos('+', start, fin);
      If cp > 0 then
      begin
        SaveOperation(_Sum);
        while (cp > start) do
        begin
          ParseLeft;
          start := cp+1;
          cp := MyPos('+', start, fin);
        end;
        cp := fin+1;
        ParseLeft;
        break;
      end;

      { seek for - }
      cp := MyPos('-', start, fin);
      If cp > 0 then
      begin
        If cp > start then
        begin
          SaveOperation(_Substract);
          ParseLeft;
          ParseRight;
          ReversePluses(cp + 1, fin);
          Break;
        end
        else
        begin { unary - }
          SaveOperation(_Unary);
          FOperands[BlockIdx].Add(AddOperand);
          ParseExpr(start + 1, fin, BlockIdx);
        end;
        break;
      end;

      { seek for % }
      cp := MyPos('%', start, fin);
      if cp > 0 then
      begin
        SaveOperation(_Mod);
        ParseLeft;
        ParseRight;
        break;
      end;

      { seek for * }
      cp := MyPos('*', start, fin);
      if cp > 0 then
      begin
        SaveOperation(_Multiply);
        ParseLeft;
        ParseRight;
        break;
      end;

      { seek for / }
      cp := MyPos('/', start, fin);
      If cp > 0 then
      begin
        SaveOperation(_Divide);
        ParseLeft;

        AddOperand;
        FOperands[BlockIdx].Add(FBlockLen);
        ReverseDiv(cp + 1, fin);
        { change * for / and vice versa }
        ParseExpr(cp + 1, fin, FBlockLen);
        break;
      end;

      { seek for ^; }
      cp := MyPos('^', start, fin);
      if cp > 0 then
      begin
        SaveOperation(_Power);
        ParseLeft;
        ParseRight;
        break;
      end;

      { seek for variables and constants }

      if CharInSet(s[start], ['''','"']) then
      begin
        SaveOperation(_Const);
        var str := ReadString(start);
        FOperands[BlockIdx].FResult := str;// Add( AddVar(str) );
        break;
      end;

      if CharInSet(s[start], digit) then
      begin
        ReadNumber(start, mynum);
        SaveOperation(_Const);
        FOperands[BlockIdx].FResult := mynum;// Add( AddVar(mynum) );
        break;
      end;

      if (s[start] = '$') or (Copy(s,start,2)='0x') then
      begin
        ReadHex(start, mynum);
        SaveOperation(_Const);
        FOperands[BlockIdx].FResult := mynum;// Add( AddVar(mynum) );
        break;
      end;

      { seek for other function names }
      { we have a function. Every func must have arg in brackets }
      { So, read ss until opening bracket: }
      cp := MyPos('(', start, fin);
      if cp <> 0 then
      begin
        st := Copy(s, start, cp - start);
        st := UpperCase(st);

        n := FFunctions.IndexOf(st);
        if (n > 0) then
        begin
          if n >= FBuildInFunctions then
            SaveOperation(_Other, n)
          else
            SaveOperation(TOperation(n));

          Repproc3;
          break;
        end;

      end; { if }

      { we have either variable by name or a special char, e.g. PI }
      n := VarIndexByName(ss);
      if n >= 0 then
      begin
        SaveOperation(_Variable);
        FOperands[BlockIdx].Add(n);
        break;
      end;

      { create variable on the fly when using assign operand }
      if (BlockIdx > 0) and (FOperands[BlockIdx-1].FOperation = _Assign) then
      begin
        n := DeclareVar(ss);
        SaveOperation(_Variable);
        FOperands[BlockIdx].Add(n);
        break;
      end;

      if SameText(ss, 'PI') then
      begin
        SaveOperation(_Const);
        FOperands[BlockIdx].FResult := PI;
        break;
      end;

      if SameText(ss, 'TRUE') then
      begin
        SaveOperation(_Const);
        FOperands[BlockIdx].FResult := True;
        break;
      end;

      if SameText(ss, 'FALSE') then
      begin
        SaveOperation(_Const);
        FOperands[BlockIdx].FResult := False;
        break;
      end;

      { unknown variable or function name
        this point is no longer reached since the parser
        declares unknown names as variables on the fly }

      SetError(1, 'Unknown keyword: '+ss, start-1, fin-start+1);

    until (FErrorCode <> 0);

  end; { proc }

begin
  Result := False;

  SetLength(FResults, 0);
  SetLength(FOperands, 0);
  //SetLength(FOperations, 0);
  SetLength(FVars, FVarNames.Count);
  FError := '';
  FErrorCode := 0;
  FErrorPos.X := 0;
  FErrorPos.Y := 0;
  FErrorLen := 0;

  s := StringReplace(s, #13#10, ';', [rfReplaceAll]);

  len := Length(s);
  FBlockLen := -1;

  { Check for errors first }
  FErrorCode := 0;
  FErrorPos.X := 0;
  FErrorPos.Y := 0;

  { unmatched brackets }
  j := 0;
  for i := 1 to len do
  begin
    if s[i] = '(' then
    begin
      inc(j);
      FErrorPos.X := i;
    end;
    if s[i] = ')' then
      dec(j);
  end;
  if j <> 0 then
    SetError(2, 'Unmatched parenthesis', 0,0)
  else
    FErrorPos.X := -1;

(*
  { invalid characters

    not relevant since we can also parse texts

  if FErrorCode <> 2 then
    for i := 1 to len do
      if not(
         (s[i] in digit) or
         (s[i] in hex) or
         (s[i] in letter) or
         (s[i] in operand) or
         (s[i] in [')', '(', '.', ',', ' ','"','''','\'])
      ) then
      begin
        FErrorCode := 3;
        FError := 'Invalid character at position '+IntToStr(i)+': "'+s[i]+'"';
      end;
  }

  { kill all spaces }

  ls := '';
  for i := 1 to len do
    if s[i] <> ' ' then
      ls := ls + Copy(s, i, 1);
  s := ls;

  len := Length(ls);

  { FResults bit of optimization: kill useless unary pluses }

  if ls[1] <> '+' then
    s := s[1]
  else
    s := '';
  for i := 2 to len do
    if (ls[i] <> '+') or (ls[i - 1] <> '(') then
      s := s + Copy(ls, i, 1);
*)

  // since the runtime parses the expression from back to front,
  // we need the first commands to appear at the back of the grammar tree.
  FErrorPos.Y := 0;
  if FErrorCode = 0 then
    ParseExpr(1, len, AddOperand);

  if FErrorCode = 0 then
  begin
    FErrorPos.X := -1; { no error found }
    Result := True;
  end;
end;

procedure TParsedFunction.SaveOperation(aOperator: TOperation; aFunctionIndex: integer = -1);
begin
  FOperands[BlockIdx].FOperation := aOperator;
  FOperands[BlockIdx].FFunctionIndex := aFunctionIndex;
end;

procedure TParsedFunction.SetError(aCode: integer; aMessage: string; aStart, aLen: integer);
begin
  FErrorCode := aCode;
  FError := aMessage;
  FErrorPos.X := aStart;
  FErrorLen := aLen;
end;

procedure TParsedFunction.SetVar(aName: string; aValue: variant);
var
  idx: integer;
begin
  aName := UpperCase(aName);
  idx := VarIndexByName(aName);

  if (idx < 0) then
  begin
    idx := FVarNames.Add(aName);
    FVarNames.Objects[idx] := Pointer( AddVar(aValue) );
  end
  else
  begin
    idx := Integer(FVarNames.Objects[idx]);
    FVars[idx] := aValue;
  end;
end;

function TParsedFunction.VarIndexByName(aName: string): integer;
begin
  aName := UpperCase(aName);
  Result := FVarNames.IndexOf(aName);
  if (Result >= 0) then
    Result := Integer(FVarNames.Objects[Result]);
end;

{ func }

{ TFunctionCallObj }

constructor TFunctionCallObj.Create(aCallBack: TFunctionCall);
begin
  FCallback := aCallBack;
end;

{ TOperand }

procedure TOperand.Add(aValue: integer);
var
  len: integer;
begin
  len := Count;
  SetLength(FParams, len+1);
  FParams[len] := aValue;
end;

function TOperand.First: integer;
begin
  Result := Params[0];
end;

function TOperand.GetParams(aIndex: integer): Integer;
begin
  if (aIndex < 0) or (aIndex >= Count) then
    raise Exception.Create('Index('+IntToStr(aIndex)+') out of range');

  Result := FParams[aIndex];
end;

function TOperand.Count: integer;
begin
  Result := Length(FParams);
end;

function TOperand.Second: integer;
begin
  Result := Params[1];
end;

end.
