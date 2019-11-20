unit X_DSA.InversePolishCalculator;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TStackOfString = TStack<string>;
  TStackOfChar = TStack<char>;
  TListOfString = TList<string>;

  TInversePolishCalculator = class
  const
    NUMBER_SET = ['0' .. '9', '.'];
    OPERATOR_SET = ['+', '-', '*', '/'];
    BRACKET_SET = ['(', ')', '[', ']'];
    LETTER_SET = ['a' .. 'z'];

  private
    __optStack, __numStack: TStackOfString;

  public
    __infixExpressionList: TListOfString; // 中缀表达式元素列表

    /// <summary> 是否有效表达式 </summary>
    function __isValidExpression(str: string): boolean;
    /// <summary> 转换字符串为表达式列表 </summary>
    procedure __trimExpression(str: string);

    constructor Create;
    destructor Destroy; override;

    function Calculator(str: string): double;

  end;

procedure Main;

implementation

procedure Main;
var
  ipc: TInversePolishCalculator;
  s: string;
begin
  s := '3*(1+2)*(4+5)的';

  ipc := TInversePolishCalculator.Create;
  //WriteLn(ipc.__isValidExpression());
  ipc.__trimExpression(s);

  for s in ipc.__infixExpressionList do
    writeLn(s, ' ');

  writeLn;
  ipc.Free;
end;

{ TInversePolishCalculator }

function TInversePolishCalculator.Calculator(str: string): double;
begin
  Result := 0.0;
end;

constructor TInversePolishCalculator.Create;
begin
  __numStack := TStackOfString.Create;
  __optStack := TStackOfString.Create;
  __infixExpressionList := TListOfString.Create;
end;

destructor TInversePolishCalculator.Destroy;
begin
  __numStack.Free;
  __optStack.Free;
  __infixExpressionList.Free;

  inherited;
end;

function TInversePolishCalculator.__isValidExpression(str: string): boolean;
var
  stack: TStackOfChar;
  c: char;
begin
  stack := TStackOfChar.Create;
  try
    for c in str do
    begin
      if (c = '(') or (c = '[') then
        stack.Push(c);

      if c = ')' then
      begin
        if (stack.Count <> 0) and (stack.Peek = '(') then
          stack.Pop
        else
          Exit(False);
      end;
    end;

    Result := stack.Count = 0;
  finally
    stack.Free;
  end;
end;

procedure TInversePolishCalculator.__trimExpression(str: string);
//const
//  NUM = ['0' .. '9', '.'];
//  OPT = ['+', '-', '*', '/'];
//  BRACKET_SET = ['(', ')', '[', ']'];
//  LETTER_SET = ['a' .. 'z'];
var
  c: char;
  charArr: TArray<char>;
  tmp: UnicodeString;
  i: integer;
begin
  if not __isValidExpression(str) then
  begin
    writeLn('Expression is Illegal.');
    Exit;
  end;

  charArr := str.ToCharArray;

  tmp := '';
  i := 0;
  while i < Length(charArr) do
  begin
    c := charArr[i];

    if CharInSet(c, NUMBER_SET) then // 当前为数字
    begin
      tmp := tmp + c;

      // i+1 是否越界
      if i + 1 < Length(charArr) then
      begin

        // 下一字符是否数字
        if CharInSet(charArr[i + 1], NUMBER_SET) then
        begin
          i := i + 1;
          Continue;
        end
        else
          __infixExpressionList.Add(Trim(tmp));
      end
      else
        __infixExpressionList.Add(Trim(tmp));
    end
    else if CharInSet(c, LETTER_SET) or CharInSet(c, BRACKET_SET) or CharInSet(c, OPERATOR_SET) then
    begin
      tmp := tmp + c;

      // i+1 是否越界
      if (i + 1 < Length(charArr)) then
      begin

        // 当前字符为字母的情况
        if (CharInSet(c, LETTER_SET)) then
        begin
          // 下一个字符是字母或者括号时
          if CharInSet(charArr[i + 1], BRACKET_SET) or CharInSet(charArr[i + 1], LETTER_SET) then
          begin
            i := i + 1;
            Continue;
          end
          else
            __infixExpressionList.Add(Trim(tmp));
        end
        else
        begin
          // 当前字符是否括号或者操作符
          if CharInSet(c, OPERATOR_SET) or CharInSet(c, BRACKET_SET) then
            __infixExpressionList.Add(Trim(tmp));
        end;
      end
      else
        __infixExpressionList.Add(Trim(tmp));
    end;

    i := i + 1;
    tmp := '';
  end;
end;

end.
