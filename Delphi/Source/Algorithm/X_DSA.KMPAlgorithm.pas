unit X_DSA.KMPAlgorithm;

interface

uses
  System.SysUtils;

type
  TArrayOfInt = array of integer;

procedure Main;
function KmpSearch(const str1, str2: UnicodeString): integer;
function KmpNext(const str: UnicodeString): TArrayOfInt;

implementation

procedure Main;
begin

end;

function KmpSearch(const str1, str2: UnicodeString): integer;
var
  KmpNext: TArrayOfInt;
begin
  KmpNext := KmpNext(str2.Length);
end;

function KmpNext(const str: UnicodeString): TArrayOfInt;
begin

end;

end.
