unit intellectmoney_base;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  TPayMethod = (pmUnspecified, pmAcquiring, pmYandexPay, pmQiwiWallet, pmSbp);
  TPreferencePayMethod = (ppmUnspecified, ppmBankCard, ppmBNPL, ppmSbp, ppmMirPay, ppmSberPay, ppmGazpromPay);
  TPreference = set of TPreferencePayMethod;
  { BankCard - банковская карта.
    BNPL - рассрочка.
    Sbp - СБП.
    MirPay - Mir Pay.
    SberPay - SberPay.
    GazpromPay - Gazprom Pay.
  }

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

function PayMethodToString(aPayMethod: TPayMethod): String;
function PreferenceToString(aPreference: TPreference): String;

implementation

const
  _PayMethods: array[TPayMethod] of String = ('', 'Acquiring', 'YandexPay', 'QiwiWallet', 'Sbp');
  _PreferencePayMethods: array[TPreferencePayMethod] of String =
    ('', 'BankCard', 'BNPL', 'Sbp', 'MirPay', 'SberPay', 'GazpromPay');

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

function PayMethodToString(aPayMethod: TPayMethod): String;
begin
  Result:=_PayMethods[aPayMethod];
end;

function PreferenceToString(aPreference: TPreference): String;
var
  i: TPreferencePayMethod;
begin
  Result:=EmptyStr;
  for i in aPreference do
    Result+=_PreferencePayMethods[i]+',';
  if Length(Result)>1 then
    SetLength(Result, Length(Result)-1);
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

