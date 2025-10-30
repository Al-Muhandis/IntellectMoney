// IntellectMoney.pas - API Wrapper for FreePascal/Lazarus
// Документация: https://wiki.intellectmoney.ru/pages/viewpage.action?pageId=160333826

unit IntellectMoney;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  fpJSON, JSONParser
  ;

type

  { TIntellectMoneyClient }

  TIntellectMoneyClient = class
  private
    FEshopId: string;
    FSecretKey: string;
    FSignSecretKey: string;
    FBearerToken: string;
    FApiUrl: string;
    FUseSSL: Boolean;
    FLastError: string;
    function CalculateHash(const AData: string; const aSignHeader: Boolean): string;
    function MakeRequest(const aMethod, aUrl, aSign: string; const aParams: TJSONObject): string;
    function ParseJSON(const AJsonStr: string): TJSONObject;
  protected
    property ApiUrl: String read FApiUrl write FApiUrl;
  public
    constructor Create(const AEshopId, ASecretKey, ASignSecretKey, ABearerToken: string);
    
    // Методы API
    function CreateInvoice(
      const AOrderId: string;
      const AAmount: Double;
      const ACurrency: string;
      const AEmail: string;
      const AUserName: string = '';
      const AServiceName: string = '';
      const ASuccessUrl: string = '';
      const AFailUrl: string = '';
      const ABackUrl: string = '';
      const AResultUrl: string = '';
      const AExpireDate: string = '';
      const AHoldMode: Integer = 0;
      const AHoldTime: Integer = 0;
      const APreference: string = ''): string;
    
    function GetPaymentState(const AInvoiceId: string): TJSONObject;
    
    function BankCardPayment(
      const AInvoiceId: string;
      const APan: string;
      const ACardHolder: string;
      const AExpiredMonth: string;
      const AExpiredYear: string;
      const ACvv: string;
      const AReturnUrl: string;
      const AIpAddress: string): TJSONObject;
    
    property EshopId: string read FEshopId write FEshopId;
    property SecretKey: string read FSecretKey write FSecretKey;
    property SignSecretKey: string read FSignSecretKey write FSignSecretKey;
    property BearerToken: string read FBearerToken write FBearerToken;
    property LastError: string read FLastError write FLastError;
  end;

implementation

uses
  md5, fphttpclient, opensslsockets, jsonscanner, fpsha256
  ;

var
  _FrmtStngsAPI: TFormatSettings;

{ TIntellectMoneyClient }

constructor TIntellectMoneyClient.Create(
  const AEshopId, ASecretKey, ASignSecretKey, ABearerToken: string);
begin
  inherited Create;
  FEshopId := AEshopId;
  FSecretKey := ASecretKey;
  FSignSecretKey := ASignSecretKey;
  FBearerToken := ABearerToken;
  FApiUrl := 'https://api.intellectmoney.ru/merchant';
  FUseSSL := True;
  FLastError := '';
end;

function TIntellectMoneyClient.CalculateHash(const aData: string; const aSignHeader: Boolean): string;
 var
  aSHA256Hash: TSHA256;
  i: Integer;
begin
  if aSignHeader then
  begin
    Result:=EmptyStr;   {
    aSHA256Hash := TSHA256.Create;
    try
      Result := LowerCase(aSHA256Hash.HashAsString(aData + FSignSecretKey));
    finally
      aSHA256Hash.Free;
    end;    }
  end
  else
    Result:=MD5Print(MD5String(aData + FSecretKey));
end;

function TIntellectMoneyClient.MakeRequest(const aMethod, aUrl, aSign: string; const aParams: TJSONObject): string;
var
  aHttp: TFPHTTPClient;
begin
  Result := '';
  aHttp := TFPHTTPClient.Create(nil);
  try
    // Установка заголовков
    aHttp.AddHeader('Content-Type', 'application/json');
    aHttp.AddHeader('Accept', 'application/json');
    if not FBearerToken.IsEmpty then
      aHttp.AddHeader('Authorization', 'Bearer ' + FBearerToken);

    if not aSign.IsEmpty then
      aHttp.AddHeader('Sign', aSign);

    if AMethod = 'POST' then
    begin
      try
        aHttp.RequestBody := TStringStream.Create(aParams.AsJSON);
        try
          Result := aHttp.Post(AUrl);
        finally
          aHttp.RequestBody.Free;
        end;
      except
        on E: Exception do
          FLastError := 'HTTP Error: ' + E.Message;
      end;
    end
    else if AMethod = 'GET' then
    begin
      try
        Result := aHttp.Get(AUrl);
      except
        on E: Exception do
          FLastError := 'HTTP Error: ' + E.Message;
      end;
    end;
  finally
    aHttp.Free;
  end;
end;

function TIntellectMoneyClient.ParseJSON(const AJsonStr: string): TJSONObject;
var
  aParser: TJSONParser;
begin
  Result := nil;
  try
    aParser := TJSONParser.Create(AJsonStr, DefaultOptions);
    try
      Result := TJSONObject(aParser.Parse);
    finally
      aParser.Free;
    end;
  except
    on E: Exception do
      FLastError := 'JSON Parse Error: ' + E.Message;
  end;
end;

function TIntellectMoneyClient.CreateInvoice(
  const AOrderId: string;
  const AAmount: Double;
  const ACurrency: string;
  const AEmail: string;
  const AUserName: string = '';
  const AServiceName: string = '';
  const ASuccessUrl: string = '';
  const AFailUrl: string = '';
  const ABackUrl: string = '';
  const AResultUrl: string = '';
  const AExpireDate: string = '';
  const AHoldMode: Integer = 0;
  const AHoldTime: Integer = 0;
  const APreference: string = ''): string;
var
  aParams: TJSONObject;
  aSignData: string;
  aHash: string;
  AmountStr, aHoldModeStr: string;
begin
  Result := '';
  aParams := TJSONObject.Create;
  
  try
    AmountStr := FormatFloat('0.00', AAmount, _FrmtStngsAPI);
    if AHoldMode=0 then
      aHoldModeStr:=EmptyStr
    else
      aHoldModeStr:=IntToStr(AHoldMode);
    
    // Подготовка данных для подписи
    aSignData := FEshopId + '::' + AOrderId + '::' + AServiceName + '::' +
               AmountStr + '::' + ACurrency + '::' + AUserName + '::' +
               AEmail + '::' + ASuccessUrl + '::' + AFailUrl + '::' +
               ABackUrl + '::' + AResultUrl + '::' + AExpireDate + '::' +
               aHoldModeStr + '::' + APreference + '::';
    
    aHash := CalculateHash(aSignData, False);
    
    // Добавление параметров
    aParams.Add('eshopId', FEshopId);
    aParams.Add('orderId', AOrderId);
    aParams.Add('recipientAmount', AmountStr);
    aParams.Add('recipientCurrency', ACurrency);
    aParams.Add('email', AEmail);
    aParams.Add('hash', aHash);
    
    if AUserName <> '' then
      aParams.Add('userName', AUserName);
    if AServiceName <> '' then
      aParams.Add('serviceName', AServiceName);
    if ASuccessUrl <> '' then
      aParams.Add('successUrl', ASuccessUrl);
    if AFailUrl <> '' then
      aParams.Add('failUrl', AFailUrl);
    if ABackUrl <> '' then
      aParams.Add('backUrl', ABackUrl);
    if AResultUrl <> '' then
      aParams.Add('resultUrl', AResultUrl);
    if AExpireDate <> '' then
      aParams.Add('expireDate', AExpireDate);
    aParams.Add('holdMode', aHoldModeStr);
    if AHoldTime > 0 then
      aParams.Add('holdTime', IntToStr(AHoldTime));
    if APreference <> '' then
      aParams.Add('preference', APreference);
    
    Result := MakeRequest('POST', FApiUrl + '/createInvoice', CalculateHash(aSignData, True), aParams);
  finally
    aParams.Free;
  end;
end;

function TIntellectMoneyClient.GetPaymentState(const AInvoiceId: string): TJSONObject;
var
  Params: TJSONObject;
  SignData: string;
  aHash: string;
  Response: string;
begin
  Result := nil;
  Params := TJSONObject.Create;
  
  try
    // Подготовка данных для подписи
    SignData := FEshopId + '::' + AInvoiceId + '::' + FSignSecretKey;
    aHash := CalculateHash(SignData, True);
    
    Params.Add('eshopId', FEshopId);
    Params.Add('invoiceId', AInvoiceId);
    Params.Add('hash', aHash);
    
    Response := MakeRequest('POST', FApiUrl + '/getbankcardpaymentstate', aHash, Params);
    Result := ParseJSON(Response);
  finally
    Params.Free;
  end;
end;

function TIntellectMoneyClient.BankCardPayment(
  const AInvoiceId: string;
  const APan: string;
  const ACardHolder: string;
  const AExpiredMonth: string;
  const AExpiredYear: string;
  const ACvv: string;
  const AReturnUrl: string;
  const AIpAddress: string): TJSONObject;
var
  Params: TJSONObject;
  aSignData: string;
  aHash: string;
  Response: string;
begin
  Result := nil;
  Params := TJSONObject.Create;
  
  try
    // Подготовка данных для подписи
    aSignData := FEshopId + '::' + AInvoiceId + '::' + APan + '::' + ACardHolder +
               '::' + AExpiredMonth + '::' + AExpiredYear + '::' + ACvv + '::' +
               AReturnUrl + '::' + AIpAddress + '::' + FSignSecretKey;
    
    aHash := CalculateHash(aSignData, True);
    
    Params.Add('eshopId', FEshopId);
    Params.Add('invoiceId', AInvoiceId);
    Params.Add('pan', APan);
    Params.Add('cardHolder', ACardHolder);
    Params.Add('expiredMonth', AExpiredMonth);
    Params.Add('expiredYear', AExpiredYear);
    Params.Add('cvv', ACvv);
    Params.Add('returnUrl', AReturnUrl);
    Params.Add('ipAddress', AIpAddress);
    Params.Add('hash', aHash);
    
    Response := MakeRequest('POST', FApiUrl + '/bankcardpayment', aHash, Params);
    Result := ParseJSON(Response);
  finally
    Params.Free;
  end;
end;

initialization
  _FrmtStngsAPI := DefaultFormatSettings;
  _FrmtStngsAPI.DecimalSeparator := '.';

end.
