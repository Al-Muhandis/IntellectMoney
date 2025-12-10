// intellectmoney_webhooks.pas - Webhook Handler for IntellectMoney
// Документация: https://wiki.intellectmoney.ru/pages/viewpage.action?pageId=4849803

unit intellectmoney_webhooks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON, JSONParser, intellectmoney_base;

type
  // Статусы платежей
  TPaymentStatus = (
    psUnknown = 0,
    psCreated = 3,           // СКО создан
    psCancelled = 4,         // СКО аннулирован
    psPaid = 5,              // СКО полностью оплачен
    psHoldPending = 6,       // Средства заблокированы (холд)
    psPartiallyPaid = 7,     // СКО частично оплачен
    psRefunded = 8           // По СКО совершен возврат
  );

  // Способы оплаты
  TPaymentMethod = (
    pmUnknown,
    pmBankCard,              // Банковская карта
    pmWebmoney,              // WebMoney
    pmYandexMoney,           // Яндекс.Деньги
    pmQIWI,                  // QIWI Wallet
    pmTerminal,              // Терминал
    pmMobilePhone,           // Мобильный телефон
    pmEInvoice,              // Электронный счет
    pmOther
  );

  // Состояние recurring операции
  TRecurringState = (
    rsNone,                  // Не используется
    rsActivated,             // Активирован
    rsDeactivated,           // Деактивирован
    rsFailed                 // Ошибка
  );

  { TWebhookNotification }
  // Класс для хранения данных уведомления
  TWebhookNotification = class
  private
    FEshopId: string;
    FPaymentId: string;
    FOrderId: string;
    FEshopAccount: string;
    FServiceName: string;
    FRecipientAmount: Double;
    FRecipientOriginalAmount: Double;
    FRefundAmount: Double;
    FRecipientCurrency: string;
    FPaymentStatus: TPaymentStatus;
    FUserName: string;
    FUserEmail: string;
    FPaymentDate: TDateTime;
    FPayMethod: string;
    FShortPan: string;
    FCountry: string;
    FBank: string;
    FIpAddress: string;
    FSecretKey: string;
    FHash: string;
    FRecurringState: string;
    FSourceInvoiceId: string;
    FUserFields: TStringList;
    FRawJSON: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;

    // Основные поля
    property EshopId: string read FEshopId write FEshopId;
    property PaymentId: string read FPaymentId write FPaymentId;
    property OrderId: string read FOrderId write FOrderId;
    property EshopAccount: string read FEshopAccount write FEshopAccount;
    property ServiceName: string read FServiceName write FServiceName;
    property RecipientAmount: Double read FRecipientAmount write FRecipientAmount;
    property RecipientOriginalAmount: Double read FRecipientOriginalAmount write FRecipientOriginalAmount;
    property RefundAmount: Double read FRefundAmount write FRefundAmount;
    property RecipientCurrency: string read FRecipientCurrency write FRecipientCurrency;
    property PaymentStatus: TPaymentStatus read FPaymentStatus write FPaymentStatus;
    property UserName: string read FUserName write FUserName;
    property UserEmail: string read FUserEmail write FUserEmail;
    property PaymentDate: TDateTime read FPaymentDate write FPaymentDate;
    property PayMethod: string read FPayMethod write FPayMethod;
    property ShortPan: string read FShortPan write FShortPan;
    property Country: string read FCountry write FCountry;
    property Bank: string read FBank write FBank;
    property IpAddress: string read FIpAddress write FIpAddress;
    property SecretKey: string read FSecretKey write FSecretKey;
    property Hash: string read FHash write FHash;
    property RecurringState: string read FRecurringState write FRecurringState;
    property SourceInvoiceId: string read FSourceInvoiceId write FSourceInvoiceId;

    // Дополнительные поля магазина
    property UserFields: TStringList read FUserFields;
    property RawJSON: TJSONObject read FRawJSON write FRawJSON;

    // Вспомогательные методы
    function GetPaymentStatusString: string;
    function IsFullyPaid: Boolean;
    function IsHoldPending: Boolean;
    function IsCancelled: Boolean;
    function IsRefunded: Boolean;
  end;

  TOnPaymentReceived = procedure(aNotification: TWebhookNotification) of object;
  TOnPaymentFailed = procedure(aNotification: TWebhookNotification; const aError: string) of object;

  { TIntellectMoneyWebhookHandler }
  TIntellectMoneyWebhookHandler = class(TIntellectMoneyBaseClient)
  private
    FLastError: string;
    FValidateIP: Boolean;
    FAllowedIP: string;

    function ParsePaymentStatus(const aStatus: string): TPaymentStatus;
    function ParseDateTime(const aDateStr: string): TDateTime;
    function ValidateIPAddress(const aRemoteIP: string): Boolean;
  protected
    function CalculateHash(const aNotification: TWebhookNotification): string;
  public
    constructor Create(const aEshopId, aSecretKey: string); override;

    // Основной метод парсинга webhook-уведомления
    function ParseWebhook(const APostData: string; out ANotification: TWebhookNotification): Boolean; overload;
    function ParseWebhook(AParams: TStrings; out ANotification: TWebhookNotification): Boolean; overload;

    // Проверка подписи
    function ValidateHash(const ANotification: TWebhookNotification): Boolean;

    // Проверка источника (IP)
    function ValidateSource(const ANotification: TWebhookNotification; const ARemoteIP: string): Boolean;

    // Формирование ответа для IntellectMoney
    function GetSuccessResponse: string;
    function GetErrorResponse(const AErrorMsg: string): string;

    property LastError: string read FLastError;
    property ValidateIP: Boolean read FValidateIP write FValidateIP;
    property AllowedIP: string read FAllowedIP write FAllowedIP;
  end;

  { TIntellectMoneyWebhookServer }
  // Класс для обработки webhook-запросов (может использоваться с веб-серверами)
  TIntellectMoneyWebhookServer = class
  private
    FHandler: TIntellectMoneyWebhookHandler;
    FOnPaymentReceived: TOnPaymentReceived;
    FOnPaymentFailed: TOnPaymentFailed;
  public
    constructor Create(const AEshopId, ASecretKey: string);
    destructor Destroy; override;

    // Обработка входящего запроса
    function ProcessRequest(const APostData: string; const ARemoteIP: string): string; overload;
    function ProcessRequest(AParams: TStrings; const ARemoteIP: string): string; overload;

    property Handler: TIntellectMoneyWebhookHandler read FHandler;
    property OnPaymentReceived: TOnPaymentReceived read FOnPaymentReceived write FOnPaymentReceived;
    property OnPaymentFailed: TOnPaymentFailed read FOnPaymentFailed write FOnPaymentFailed;
  end;

implementation

uses
  md5, DateUtils;

var
  _FrmtStngs: TFormatSettings;

{ TWebhookNotification }

constructor TWebhookNotification.Create;
begin
  inherited Create;
  FUserFields := TStringList.Create;
  FPaymentStatus := psUnknown;
  FRecipientAmount := 0;
  FRecipientOriginalAmount := 0;
  FRefundAmount := 0;
end;

destructor TWebhookNotification.Destroy;
begin
  FUserFields.Free;
  FRawJSON.Free;
  inherited Destroy;
end;

function TWebhookNotification.GetPaymentStatusString: string;
begin
  case FPaymentStatus of
    psCreated: Result := 'СКО создан';
    psCancelled: Result := 'СКО аннулирован';
    psPaid: Result := 'СКО полностью оплачен';
    psHoldPending: Result := 'Средства заблокированы (холдирование)';
    psPartiallyPaid: Result := 'СКО частично оплачен';
    psRefunded: Result := 'По СКО совершен возврат';
  else
    Result := 'Неизвестный статус';
  end;
end;

function TWebhookNotification.IsFullyPaid: Boolean;
begin
  Result := FPaymentStatus = psPaid;
end;

function TWebhookNotification.IsHoldPending: Boolean;
begin
  Result := FPaymentStatus = psHoldPending;
end;

function TWebhookNotification.IsCancelled: Boolean;
begin
  Result := FPaymentStatus = psCancelled;
end;

function TWebhookNotification.IsRefunded: Boolean;
begin
  Result := FPaymentStatus = psRefunded;
end;

{ TIntellectMoneyWebhookHandler }

constructor TIntellectMoneyWebhookHandler.Create(const aEshopId, aSecretKey: string);
begin
  inherited Create(AEshopId, ASecretKey);
  FValidateIP := False;
  FAllowedIP := '';
  FLastError := '';
end;

function TIntellectMoneyWebhookHandler.ParsePaymentStatus(const aStatus: string): TPaymentStatus;
var
  StatusInt: Integer;
begin
  Result := psUnknown;
  if TryStrToInt(AStatus, StatusInt) then
  begin
    case StatusInt of
      3: Result := psCreated;
      4: Result := psCancelled;
      5: Result := psPaid;
      6: Result := psHoldPending;
      7: Result := psPartiallyPaid;
      8: Result := psRefunded;
    end;
  end;
end;

function TIntellectMoneyWebhookHandler.ParseDateTime(const aDateStr: string): TDateTime;
begin
  // Формат: "yyyy-MM-dd HH:mm:ss"
  try
    Result := ScanDateTime('yyyy-mm-dd hh:nn:ss', aDateStr);
  except
    Result := Now;
  end;
end;

function TIntellectMoneyWebhookHandler.CalculateHash(const aNotification: TWebhookNotification): string;
var
  aSignString: string;
  aAmountStr: string;
begin
  // Формирование строки для подписи согласно документации
  // eshopId::orderId::serviceName::eshopAccount::recipientAmount::recipientCurrency::
  // paymentStatus::userName::userEmail::paymentData::secretKey

  aAmountStr := FormatFloat('0.00', aNotification.RecipientAmount, _FrmtStngs);

  aSignString := aNotification.EshopId + '::' +
                aNotification.OrderId + '::' +
                aNotification.ServiceName + '::' +
                aNotification.EshopAccount + '::' +
                aAmountStr + '::' +
                aNotification.RecipientCurrency + '::' +
                IntToStr(Ord(aNotification.PaymentStatus)) + '::' +
                aNotification.UserName + '::' +
                aNotification.UserEmail + '::' +
                FormatDateTime('yyyy-mm-dd hh:nn:ss', aNotification.PaymentDate) + '::' +
                SecretKey;

  Result := MD5Print(MD5String(aSignString));
end;

function TIntellectMoneyWebhookHandler.ValidateIPAddress(const aRemoteIP: string): Boolean;
begin
  Result := (Pos(FAllowedIP, ARemoteIP) > 0) or (not FValidateIP);
end;

function TIntellectMoneyWebhookHandler.ParseWebhook(const APostData: string;
  out ANotification: TWebhookNotification): Boolean;
var
  Params: TStringList;
begin
  Params := TStringList.Create;
  try
    // Парсинг POST данных (формат: key=value&key2=value2)
    Params.Delimiter := '&';
    Params.StrictDelimiter := True;
    Params.DelimitedText := APostData;

    Result := ParseWebhook(Params, ANotification);
  finally
    Params.Free;
  end;
end;

function TIntellectMoneyWebhookHandler.ParseWebhook(AParams: TStrings;
  out ANotification: TWebhookNotification): Boolean;
var
  i: Integer;
  ParamName, ParamValue: string;
  AmountStr: string;
begin
  Result := False;
  ANotification := TWebhookNotification.Create;
  FLastError := '';

  try
    // Парсинг обязательных полей
    ANotification.EshopId := AParams.Values['eshopId'];
    ANotification.PaymentId := AParams.Values['paymentId'];
    ANotification.OrderId := AParams.Values['orderId'];
    ANotification.EshopAccount := AParams.Values['eshopAccount'];
    ANotification.ServiceName := AParams.Values['serviceName'];

    // Парсинг сумм
    AmountStr := StringReplace(AParams.Values['recipientAmount'], ',', '.', [rfReplaceAll]);
    if not TryStrToFloat(AmountStr, ANotification.FRecipientAmount, _FrmtStngs) then
      ANotification.FRecipientAmount := 0;

    AmountStr := StringReplace(AParams.Values['recipientOriginalAmount'], ',', '.', [rfReplaceAll]);
    if not TryStrToFloat(AmountStr, ANotification.FRecipientOriginalAmount, _FrmtStngs) then
      ANotification.FRecipientOriginalAmount := ANotification.RecipientAmount;

    AmountStr := StringReplace(AParams.Values['refundAmount'], ',', '.', [rfReplaceAll]);
    if not TryStrToFloat(AmountStr, ANotification.FRefundAmount, _FrmtStngs) then
      ANotification.FRefundAmount := 0;

    ANotification.RecipientCurrency := AParams.Values['recipientCurrency'];
    ANotification.PaymentStatus := ParsePaymentStatus(AParams.Values['paymentStatus']);
    ANotification.UserName := AParams.Values['userName'];
    ANotification.UserEmail := AParams.Values['userEmail'];
    ANotification.PaymentDate := ParseDateTime(AParams.Values['paymentData']);

    // Дополнительные поля
    ANotification.PayMethod := AParams.Values['payMethod'];
    ANotification.ShortPan := AParams.Values['shortPan'];
    ANotification.Country := AParams.Values['country'];
    ANotification.Bank := AParams.Values['bank'];
    ANotification.IpAddress := AParams.Values['ipAddress'];
    ANotification.SecretKey := AParams.Values['secretKey'];
    ANotification.Hash := AParams.Values['hash'];
    ANotification.RecurringState := AParams.Values['reccurringState'];
    ANotification.SourceInvoiceId := AParams.Values['sourceInvoiceId'];

    // Парсинг пользовательских полей (UserField_N, UserFieldName_N)
    for i := 0 to AParams.Count - 1 do
    begin
      ParamName := AParams.Names[i];
      ParamValue := AParams.ValueFromIndex[i];

      if (Pos('UserField_', ParamName) = 1) or (Pos('UserFieldName_', ParamName) = 1) then
        ANotification.UserFields.AddPair(ParamName, ParamValue);
    end;

    // Проверка обязательных полей
    if (ANotification.EshopId = '') or (ANotification.OrderId = '') then
    begin
      FLastError := 'Отсутствуют обязательные поля';
      Exit(False);
    end;

    Result := True;
  except
    on E: Exception do
    begin
      FLastError := 'Ошибка парсинга: ' + E.Message;
      FreeAndNil(ANotification);
    end;
  end;
end;

function TIntellectMoneyWebhookHandler.ValidateHash(const ANotification: TWebhookNotification): Boolean;
var
  CalculatedHash: string;
begin
  CalculatedHash := CalculateHash(ANotification);
  Result := LowerCase(CalculatedHash) = LowerCase(ANotification.Hash);

  if not Result then
    FLastError := 'Неверная подпись (hash)';
end;

function TIntellectMoneyWebhookHandler.ValidateSource(const ANotification: TWebhookNotification;
  const ARemoteIP: string): Boolean;
begin
  Result := ValidateIPAddress(ARemoteIP);

  if not Result then
    FLastError := 'IP адрес ' + ARemoteIP + ' не разрешен';
end;

function TIntellectMoneyWebhookHandler.GetSuccessResponse: string;
begin
  Result := 'OK';
end;

function TIntellectMoneyWebhookHandler.GetErrorResponse(const AErrorMsg: string): string;
begin
  Result := 'ERROR: ' + AErrorMsg;
end;

{ TIntellectMoneyWebhookServer }

constructor TIntellectMoneyWebhookServer.Create(const AEshopId, ASecretKey: string);
begin
  inherited Create;
  FHandler := TIntellectMoneyWebhookHandler.Create(AEshopId, ASecretKey);
end;

destructor TIntellectMoneyWebhookServer.Destroy;
begin
  FHandler.Free;
  inherited Destroy;
end;

function TIntellectMoneyWebhookServer.ProcessRequest(const APostData: string;
  const ARemoteIP: string): string;
var
  aNotification: TWebhookNotification;
begin
  aNotification := nil;

  try
    // Парсинг webhook
    if not FHandler.ParseWebhook(APostData, aNotification) then
    begin
      if Assigned(FOnPaymentFailed) then
        FOnPaymentFailed(nil, FHandler.LastError);
      Exit(FHandler.GetErrorResponse(FHandler.LastError));
    end;

    // Проверка IP
    if not FHandler.ValidateSource(aNotification, ARemoteIP) then
    begin
      if Assigned(FOnPaymentFailed) then
        FOnPaymentFailed(aNotification, FHandler.LastError);
      Exit(FHandler.GetErrorResponse(FHandler.LastError));
    end;

    // Проверка подписи
    if not FHandler.ValidateHash(aNotification) then
    begin
      if Assigned(FOnPaymentFailed) then
        FOnPaymentFailed(aNotification, FHandler.LastError);
      Exit(FHandler.GetErrorResponse(FHandler.LastError));
    end;

    // Вызов обработчика успешного платежа
    if Assigned(FOnPaymentReceived) then
      FOnPaymentReceived(aNotification);

    Result := FHandler.GetSuccessResponse;
  finally
    aNotification.Free;
  end;
end;

function TIntellectMoneyWebhookServer.ProcessRequest(AParams: TStrings;
  const ARemoteIP: string): string;
var
  Notification: TWebhookNotification;
begin
  Notification := nil;

  try
    // Парсинг webhook
    if not FHandler.ParseWebhook(AParams, Notification) then
    begin
      if Assigned(FOnPaymentFailed) then
        FOnPaymentFailed(nil, FHandler.LastError);
      Exit(FHandler.GetErrorResponse(FHandler.LastError));
    end;

    // Проверка IP
    if not FHandler.ValidateSource(Notification, ARemoteIP) then
    begin
      if Assigned(FOnPaymentFailed) then
        FOnPaymentFailed(Notification, FHandler.LastError);
      Exit(FHandler.GetErrorResponse(FHandler.LastError));
    end;

    // Проверка подписи
    if not FHandler.ValidateHash(Notification) then
    begin
      if Assigned(FOnPaymentFailed) then
        FOnPaymentFailed(Notification, FHandler.LastError);
      Exit(FHandler.GetErrorResponse(FHandler.LastError));
    end;

    // Вызов обработчика успешного платежа
    if Assigned(FOnPaymentReceived) then
      FOnPaymentReceived(Notification);

    Result := FHandler.GetSuccessResponse;
  finally
    if Assigned(Notification) then
      Notification.Free;
  end;
end;

initialization
  _FrmtStngs := DefaultFormatSettings;
  _FrmtStngs.DecimalSeparator := '.';

end.

