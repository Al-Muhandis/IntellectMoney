// IntellectMoney.pas - API Wrapper for FreePascal/Lazarus
// Документация: https://wiki.intellectmoney.ru/pages/viewpage.action?pageId=4849803

unit intellectmoney_merchant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intellectmoney_base
  ;

type

  { TIntellectMoneyMerchantClient }

  TIntellectMoneyMerchantClient = class(TIntellectMoneyBaseClient)
  private
    function GenerateHash(const aOrderId, aServiceName, aAmount, aCurrency: string): String;
  public
    constructor Create(const aEshopId, aSecretKey: string); override;
    function CreatePaymentURL(
      const aOrderId: string;
      const aAmount: Double;
      const aCurrency: string;
      const aEmail: string;
      const aUserName: string = '';
      const aServiceName: string = '';
      const aSuccessUrl: string = '';
      const aFailUrl: string = '';
      const aBackURL: string = ''): string;
  end;

implementation

uses
  md5, fphttpclient, opensslsockets, jsonscanner
  ;

var
  _FrmtStngsAPI: TFormatSettings;

function EncodeURLParams(aParams: TStringList): string;
var
  i: Integer;
  aEncoded: TStringList;
begin
  aEncoded := TStringList.Create;
  aEncoded.Delimiter:='&';
  try
    for i := 0 to aParams.Count - 1 do
      aEncoded.AddPair(aParams.Names[i], EncodeURLElement(aParams.ValueFromIndex[i]));
    Result := aEncoded.DelimitedText;
  finally
    aEncoded.Free;
  end;
end;

{ TIntellectMoneyMerchantClient }

function TIntellectMoneyMerchantClient.GenerateHash(const aOrderId, aServiceName, aAmount, aCurrency: string): String;
var
  aSignString: string;
begin
  aSignString := EshopId+'::'+aOrderId+'::'+aServiceName+'::'+aAmount+'::'+aCurrency+'::'+SecretKey;
  Result := MD5Print(MD5String(aSignString));
end;

constructor TIntellectMoneyMerchantClient.Create(const aEshopId, aSecretKey: string);
begin
  inherited Create(aEshopId, aSecretKey);
  Url:='https://merchant.intellectmoney.ru/%s/';
end;

function TIntellectMoneyMerchantClient.CreatePaymentURL(const aOrderId: string; const aAmount: Double;
  const aCurrency: string; const aEmail: string; const aUserName: string; const aServiceName: string;
  const aSuccessUrl: string; const aFailUrl: string; const aBackURL: string): string;
var
  aParams: TStringList;
  aHash, aAmountStr: string;
begin
  aParams := TStringList.Create;
  try
    aAmountStr := FormatFloat('0.00', AAmount, _FrmtStngsAPI);

    aParams.Values['eshopId'] :=           EshopId;
    aParams.Values['orderId'] :=           aOrderId;
    aParams.Values['serviceName'] :=       aServiceName;
    aParams.Values['recipientAmount'] :=   aAmountStr;
    aParams.Values['recipientCurrency'] := aCurrency;
    aParams.Values['userName'] :=          aUserName;
    aParams.Values['userEmail'] :=         aEmail;
    aParams.Values['successUrl'] :=        aSuccessUrl;
    aParams.Values['failUrl'] :=           aFailUrl;
    aParams.Values['backUrl'] :=           aBackURL;

    aHash := GenerateHash(
      aParams.Values['orderId'],
      aParams.Values['serviceName'],
      aParams.Values['recipientAmount'],
      aParams.Values['recipientCurrency']
    );
    aParams.Values['hash'] := aHash;

    Result := Format(Url, [Lang]) +'?'+EncodeURLParams(aParams);
  finally
    aParams.Free;
  end;
end;

initialization
  _FrmtStngsAPI := DefaultFormatSettings;
  _FrmtStngsAPI.DecimalSeparator := '.';

end.
