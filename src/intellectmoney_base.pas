unit intellectmoney_base;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TIntellectMoneyBaseClient }

  TIntellectMoneyBaseClient = class
   private
     FEshopId: string;
     FLang: String;
     FSecretKey: string;
     FBaseUrl: string;
   protected
     property Url: String read FBaseUrl write FBaseUrl;
     property Lang: String read FLang write FLang;
   public
     constructor Create(const aEshopId, aSecretKey: string); virtual;
     property EshopId: string read FEshopId write FEshopId;
     property SecretKey: string read FSecretKey write FSecretKey;
   end;

function JoinForKey(const aElements: array of String): String;

implementation

function JoinForKey(const aElements: array of String): String;
var
  aKey: String;
begin
  Result:=EmptyStr;
  for aKey in aElements do
    Result+=aKey+'::';
  if not Result.IsEmpty then
    Result:=Copy(Result, 1, Length(Result) - 2);
end;

{ TIntellectMoneyBaseClient }

constructor TIntellectMoneyBaseClient.Create(const aEshopId, aSecretKey: string);
begin
  inherited Create;
  FEshopId := AEshopId;
  FSecretKey := ASecretKey;
  FLang:='ru';
end;

end.

