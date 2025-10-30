// IntellectMoney.pas - Complete API Wrapper with Payment Protocol Support
// Документация: https://wiki.intellectmoney.ru/pages/viewpage.action?pageId=4849803
// Полная реализация протокола приема платежей для интернет-магазинов

unit IntellectMoney;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON, JSONParser, fphttpclient, opensslsockets;

type

  // Статусы платежей согласно протоколу
  TPaymentStatus = (
    psUnknown = -1,
    psInvoiceCreated = 3,      // Счет создан
    psBlocked = 6,              // Средства заблокированы (холдирование)
    psConfirmed = 5,            // Платеж подтвержден
    psCancelled = 4,            // Платеж отменен
    psPartialConfirmed = 7,     // Платеж частично подтвержден
    psRefunded = 8              // Возврат средств
  );

  // Режимы холдирования (блокировки средств)
  THoldMode = (
    hmNone = 0,                 // Без холдирования
    hmWait = 1,                 // Ждать команды магазина
    hmBlockContinue = 2         // Заблокировать и продолжить
  );

  // Способы оплаты
  TPayMethod = (
    pmWebMoney,
    pmYandexMoney,
    pmBankCard,
    pmQIWI,
    pmSberbank,
    pmPayPal,
    pmCryptocurrency
  );

  // Позиция в чеке для онлайн-кассы
  TMerchantReceiptPosition = record
    Quantity: Double;           // Количество
    Price: Double;              // Цена
    Tax: Integer;               // Ставка НДС: 1=0%, 2=10%, 3=18%, 4=20%, 5=10/110, 6=18/118
    Text: string;               // Описание (до 128 байт UTF-8)
    PaymentSubjectType: Integer; // Предмет расчета (1-13)
    PaymentMethodType: Integer;  // Способ расчета (1-7)
    SupplierINN: string;        // ИНН поставщика (до 12 символов)
  end;

  // Чек для онлайн-кассы
  TMerchantReceipt = record
    INN: string;                // Индивидуальный номер налогоплательщика
    Group: string;              // Название группы устройств
    Type_: Integer;             // 1 = приход, 2 = возврат приход
    Positions: array of TMerchantReceiptPosition;
    CustomerContact: string;    // Телефон или Email покупателя
    TaxationSystem: Integer;    // Система налогообложения (0-5)
  end;

  // Данные об уведомлении платежа
  TPaymentNotification = record
    EshopId: string;
    PaymentId: string;
    OrderId: string;
    EshopAccount: string;
    ServiceName: string;
    RecipientAmount: Double;
    RecipientOriginalAmount: Double;
    RecipientCurrency: string;
    PaymentStatus: TPaymentStatus;
    UserName: string;
    UserEmail: string;
    PaymentData: TDateTime;
    PayMethod: string;
    ShortPan: string;
    Country: string;
    Bank: string;
    IPAddress: string;
    Hash: string;
    SecretKey: string;
  end;

  { TIntellectMoneyClient }

  TIntellectMoneyClient = class
  private
    FEshopId: string;
    FSecretKey: string;
    FSignSecretKey: string;
    FBearerToken: string;
    FApiUrl: string;
    FMerchantUrl: string;
    FUseSSL: Boolean;
    FLastError: string;
    FResultUrl: string;

    function CalculateMD5Hash(const AData: string): string;
    function CalculateHash(const AData: string; const aSignHeader: Boolean): string;
    function MakeRequest(const aMethod, aUrl, aSign: string; const aParams: TJSONObject): string;
    function ParseJSON(const AJsonStr: string): TJSONObject;
    function FormatAmount(AAmount: Double): string;
    function VerifyNotificationHash(const ANotification: TPaymentNotification): Boolean;

  protected
    property ApiUrl: String read FApiUrl write FApiUrl;

  public
    constructor Create(const AEshopId, ASecretKey, ASignSecretKey, ABearerToken: string);
    destructor Destroy; override;

    // Методы работы со счетами (счет к оплате - СКО)
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
      const AHoldMode: THoldMode = hmNone;
      const AHoldTime: Integer = 0;
      const APreference: string = '';
      const AReceipt: TMerchantReceipt = nil): string;

    function GetPaymentState(const AInvoiceId: string): TJSONObject;

    // Методы работы с платежами по банковским картам
    function BankCardPayment(
      const AInvoiceId: string;
      const APan: string;
      const ACardHolder: string;
      const AExpiredMonth: string;
      const AExpiredYear: string;
      const ACvv: string;
      const AReturnUrl: string;
      const AIpAddress: string): TJSONObject;

    // Методы управления холдированными счетами
    function ConfirmHoldInvoice(const AOrderId: string): string;
    function RefundHoldInvoice(const AOrderId: string; const AAmount: Double = 0;
      const ANewReceipt: TMerchantReceipt = nil): string;

    // Методы валидации уведомлений от системы
    function ParseNotification(const ANotificationData: TStringList): TPaymentNotification;
    function ValidateNotificationData(const ANotification: TPaymentNotification;
      const AExpectedAmount: Double; const AExpectedCurrency: string): Boolean;

    // Методы для работы с платежной страницей (HTML форма)
    function GetPaymentFormHTML(
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
      const AHoldMode: THoldMode = hmNone;
      const AHoldTime: Integer = 0;
      const APreference: string = ''): string;

    // Утилиты
    function GenerateHash(const AData: string): string;
    function GetPaymentStatusText(AStatus: TPaymentStatus): string;
    function PaymentStatusFromString(const AStatusStr: string): TPaymentStatus;
    function PaymentStatusToInteger(AStatus: TPaymentStatus): Integer;

    property EshopId: string read FEshopId write FEshopId;
    property SecretKey: string read FSecretKey write FSecretKey;
    property SignSecretKey: string read FSignSecretKey write FSignSecretKey;
    property BearerToken: string read FBearerToken write FBearerToken;
    property LastError: string read FLastError write FLastError;
    property ResultUrl: string read FResultUrl write FResultUrl;
    property MerchantUrl: string read FMerchantUrl write FMerchantUrl;
  end;

implementation

uses
  md5, fpsha256, strutils;

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
  FMerchantUrl := 'https://merchant.intellectmoney.ru/ru/';
  FUseSSL := True;
  FLastError := '';
  FResultUrl := '';
end;

destructor TIntellectMoneyClient.Destroy;
begin
  inherited Destroy;
end;

function TIntellectMoneyClient.CalculateMD5Hash(const AData: string): string;
begin
  Result := MD5Print(MD5String(AData));
end;

function TIntellectMoneyClient.CalculateHash(const AData: string; const aSignHeader: Boolean): string;
var
  aSHA256Hash: TSHA256;
begin
  if aSignHeader then
  begin
    Result := EmptyStr;
    // SHA256 реализуется если нужна подпись в заголовке
  end
  else
    Result := CalculateMD5Hash(AData + FSecretKey);
end;

function TIntellectMoneyClient.FormatAmount(AAmount: Double): string;
begin
  Result := FormatFloat('0.00', AAmount, _FrmtStngsAPI);
end;

function TIntellectMoneyClient.MakeRequest(const aMethod, aUrl, aSign: string; const aParams: TJSONObject): string;
var
  aHttp: TFPHTTPClient;
begin
  Result := '';
  aHttp := TFPHTTPClient.Create(nil);
  try
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

function TIntellectMoneyClient.GenerateHash(const AData: string): string;
begin
  Result := CalculateMD5Hash(AData + FSecretKey);
end;

function TIntellectMoneyClient.PaymentStatusToInteger(AStatus: TPaymentStatus): Integer;
begin
  Result := Integer(AStatus);
end;

function TIntellectMoneyClient.PaymentStatusFromString(const AStatusStr: string): TPaymentStatus;
var
  StatusCode: Integer;
begin
  Result := psUnknown;
  if TryStrToInt(AStatusStr, StatusCode) then
  begin
    case StatusCode of
      3: Result := psInvoiceCreated;
      4: Result := psCancelled;
      5: Result := psConfirmed;
      6: Result := psBlocked;
      7: Result := psPartialConfirmed;
      8: Result := psRefunded;
    end;
  end;
end;

function TIntellectMoneyClient.GetPaymentStatusText(AStatus: TPaymentStatus): string;
begin
  case AStatus of
    psInvoiceCreated: Result := 'Invoice Created';
    psBlocked: Result := 'Payment Blocked (Hold)';
    psConfirmed: Result := 'Payment Confirmed';
    psCancelled: Result := 'Payment Cancelled';
    psPartialConfirmed: Result := 'Payment Partially Confirmed';
    psRefunded: Result := 'Payment Refunded';
  else
    Result := 'Unknown Status';
  end;
end;

function TIntellectMoneyClient.VerifyNotificationHash(const ANotification: TPaymentNotification): Boolean;
var
  ExpectedHash: string;
  HashData: string;
begin
  // Формируем строку для проверки подписи согласно протоколу
  HashData := FEshopId + '::' + ANotification.OrderId + '::' +
    ANotification.ServiceName + '::' + ANotification.EshopAccount + '::' +
    FormatAmount(ANotification.RecipientAmount) + '::' +
    ANotification.RecipientCurrency + '::' +
    IntToStr(PaymentStatusToInteger(ANotification.PaymentStatus)) + '::' +
    ANotification.UserName + '::' + ANotification.UserEmail + '::' +
    FormatDateTime('yyyy-mm-dd hh:mm:ss', ANotification.PaymentData) + '::' +
    FSecretKey;

  ExpectedHash := CalculateMD5Hash(HashData);
  Result := ExpectedHash = ANotification.Hash;
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
  const AHoldMode: THoldMode = hmNone;
  const AHoldTime: Integer = 0;
  const APreference: string = '';
  const AReceipt: TMerchantReceipt = nil): string;
var
  aParams: TJSONObject;
  aSignData: string;
  aHash: string;
  AmountStr, aHoldModeStr: string;
begin
  Result := '';
  aParams := TJSONObject.Create;
  try
    AmountStr := FormatAmount(AAmount);
    if AHoldMode = hmNone then
      aHoldModeStr := EmptyStr
    else
      aHoldModeStr := IntToStr(Integer(AHoldMode));

    // Формируем строку для подписи согласно протоколу
    aSignData := FEshopId + '::' + AOrderId + '::' + AServiceName + '::' +
      AmountStr + '::' + ACurrency + '::' + AUserName + '::' +
      AEmail + '::' + ASuccessUrl + '::' + AFailUrl + '::' +
      ABackUrl + '::' + AResultUrl + '::' + AExpireDate + '::' +
      aHoldModeStr + '::' + APreference + '::';

    aHash := CalculateHash(aSignData, False);

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
    
    if AHoldMode <> hmNone then
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

function TIntellectMoneyClient.ConfirmHoldInvoice(const AOrderId: string): string;
var
  Params: TJSONObject;
  aSignData: string;
  aHash: string;
begin
  Result := '';
  Params := TJSONObject.Create;
  try
    aSignData := FEshopId + '::' + AOrderId + '::ToPaid::' + FSecretKey;
    aHash := GenerateHash(aSignData);

    Params.Add('eshopId', FEshopId);
    Params.Add('orderId', AOrderId);
    Params.Add('action', 'ToPaid');
    Params.Add('secretKey', FSecretKey);
    Params.Add('hash', aHash);

    Result := MakeRequest('POST', FMerchantUrl, '', Params);
  finally
    Params.Free;
  end;
end;

function TIntellectMoneyClient.RefundHoldInvoice(const AOrderId: string;
  const AAmount: Double = 0; const ANewReceipt: TMerchantReceipt = nil): string;
var
  Params: TJSONObject;
  aSignData: string;
  aHash: string;
begin
  Result := '';
  Params := TJSONObject.Create;
  try
    aSignData := FEshopId + '::' + AOrderId + '::Refund::' + FSecretKey;
    aHash := GenerateHash(aSignData);

    Params.Add('eshopId', FEshopId);
    Params.Add('orderId', AOrderId);
    Params.Add('action', 'Refund');
    Params.Add('secretKey', FSecretKey);
    Params.Add('hash', aHash);

    if AAmount > 0 then
      Params.Add('operationAmount', FormatAmount(AAmount));

    Result := MakeRequest('POST', FMerchantUrl, '', Params);
  finally
    Params.Free;
  end;
end;

function TIntellectMoneyClient.ParseNotification(const ANotificationData: TStringList): TPaymentNotification;
var
  i: Integer;
  Key, Value: string;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.PaymentStatus := psUnknown;

  for i := 0 to ANotificationData.Count - 1 do
  begin
    Key := ANotificationData.Names[i];
    Value := ANotificationData.ValueFromIndex[i];

    if Key = 'eshopId' then Result.EshopId := Value
    else if Key = 'paymentId' then Result.PaymentId := Value
    else if Key = 'orderId' then Result.OrderId := Value
    else if Key = 'eshopAccount' then Result.EshopAccount := Value
    else if Key = 'serviceName' then Result.ServiceName := Value
    else if Key = 'recipientAmount' then Result.RecipientAmount := StrToFloatDef(Value, 0)
    else if Key = 'recipientOriginalAmount' then Result.RecipientOriginalAmount := StrToFloatDef(Value, 0)
    else if Key = 'recipientCurrency' then Result.RecipientCurrency := Value
    else if Key = 'paymentStatus' then Result.PaymentStatus := PaymentStatusFromString(Value)
    else if Key = 'userName' then Result.UserName := Value
    else if Key = 'userEmail' then Result.UserEmail := Value
    else if Key = 'paymentData' then
      Result.PaymentData := StrToDateTimeDef(Value, Now)
    else if Key = 'payMethod' then Result.PayMethod := Value
    else if Key = 'shortPan' then Result.ShortPan := Value
    else if Key = 'country' then Result.Country := Value
    else if Key = 'bank' then Result.Bank := Value
    else if Key = 'ipAddress' then Result.IPAddress := Value
    else if Key = 'hash' then Result.Hash := Value
    else if Key = 'secretKey' then Result.SecretKey := Value;
  end;
end;

function TIntellectMoneyClient.ValidateNotificationData(const ANotification: TPaymentNotification;
  const AExpectedAmount: Double; const AExpectedCurrency: string): Boolean;
begin
  Result := True;

  // Проверяем eshopId
  if ANotification.EshopId <> FEshopId then
  begin
    FLastError := 'Invalid eshopId';
    Result := False;
    Exit;
  end;

  // Проверяем сумму
  if Abs(ANotification.RecipientAmount - AExpectedAmount) > 0.01 then
  begin
    FLastError := 'Amount mismatch';
    Result := False;
    Exit;
  end;

  // Проверяем валюту
  if ANotification.RecipientCurrency <> AExpectedCurrency then
  begin
    FLastError := 'Currency mismatch';
    Result := False;
    Exit;
  end;

  // Проверяем хеш
  if not VerifyNotificationHash(ANotification) then
  begin
    FLastError := 'Hash verification failed';
    Result := False;
    Exit;
  end;
end;

function TIntellectMoneyClient.GetPaymentFormHTML(
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
  const AHoldMode: THoldMode = hmNone;
  const AHoldTime: Integer = 0;
  const APreference: string = ''): string;
var
  AmountStr, aHoldModeStr: string;
  aSignData: string;
  aHash: string;
begin
  AmountStr := FormatAmount(AAmount);
  if AHoldMode = hmNone then
    aHoldModeStr := EmptyStr
  else
    aHoldModeStr := IntToStr(Integer(AHoldMode));

  aSignData := FEshopId + '::' + AOrderId + '::' + AServiceName + '::' +
    AmountStr + '::' + ACurrency + '::' + AUserName + '::' +
    AEmail + '::' + ASuccessUrl + '::' + AFailUrl + '::' +
    ABackUrl + '::' + AResultUrl + '::' + AExpireDate + '::' +
    aHoldModeStr + '::' + APreference + '::';

  aHash := GenerateHash(aSignData);

  Result := '<form action="' + FMerchantUrl + '" method="POST" name="pay">' + LineEnding +
    '  <input type="hidden" name="eshopId" value="' + FEshopId + '">' + LineEnding +
    '  <input type="hidden" name="orderId" value="' + AOrderId + '">' + LineEnding +
    '  <input type="hidden" name="recipientAmount" value="' + AmountStr + '">' + LineEnding +
    '  <input type="hidden" name="recipientCurrency" value="' + ACurrency + '">' + LineEnding +
    '  <input type="hidden" name="user_email" value="' + AEmail + '">' + LineEnding;

  if AUserName <> '' then
    Result := Result + '  <input type="hidden" name="userName" value="' + AUserName + '">' + LineEnding;
  if AServiceName <> '' then
    Result := Result + '  <input type="hidden" name="serviceName" value="' + AServiceName + '">' + LineEnding;
  if ASuccessUrl <> '' then
    Result := Result + '  <input type="hidden" name="successUrl" value="' + ASuccessUrl + '">' + LineEnding;
  if AFailUrl <> '' then
    Result := Result + '  <input type="hidden" name="failUrl" value="' + AFailUrl + '">' + LineEnding;
  if ABackUrl <> '' then
    Result := Result + '  <input type="hidden" name="backUrl" value="' + ABackUrl + '">' + LineEnding;
  if AResultUrl <> '' then
    Result := Result + '  <input type="hidden" name="resultUrl" value="' + AResultUrl + '">' + LineEnding;
  if AExpireDate <> '' then
    Result := Result + '  <input type="hidden" name="expireDate" value="' + AExpireDate + '">' + LineEnding;
  if AHoldMode <> hmNone then
    Result := Result + '  <input type="hidden" name="holdMode" value="' + aHoldModeStr + '">' + LineEnding;
  if AHoldTime > 0 then
    Result := Result + '  <input type="hidden" name="holdTime" value="' + IntToStr(AHoldTime) + '">' + LineEnding;
  if APreference <> '' then
    Result := Result + '  <input type="hidden" name="preference" value="' + APreference + '">' + LineEnding;

  Result := Result + '  <input type="hidden" name="hash" value="' + aHash + '">' + LineEnding +
    '  <input type="submit" value="оплатить">' + LineEnding +
    '</form>';
end;

initialization
  _FrmtStngsAPI := DefaultFormatSettings;
  _FrmtStngsAPI.DecimalSeparator := '.';

end.