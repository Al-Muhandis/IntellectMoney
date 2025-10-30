// ============================================================================
// IntellectMoneyClientTests.pas - Tests for TIntellectMoneyClient
// ============================================================================

unit IntellectMoneyClientTests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, IntellectMoney, IntellectMoneyTests, MockServer, fpJSON
  ;

type
  TIntellectMoneyClientTest = class(TIntellectMoneyTestCase)
  published
    procedure TestClientCreation;
    procedure TestCreateInvoiceBasic;
    procedure TestCreateInvoiceWithAllParameters;
    procedure TestCreateInvoiceValidation;
    procedure TestGetPaymentState;
    procedure TestHashCalculation;
    procedure TestErrorHandling;
  end;

implementation

{ TIntellectMoneyClientTest }

procedure TIntellectMoneyClientTest.TestClientCreation;
begin
  AssertEqualsString('12345', FClient.EshopId, 'EshopId should match');
  AssertEqualsString('test_secret_key', FClient.SecretKey, 'SecretKey should match');
  AssertEqualsString('test_sign_secret_key', FClient.SignSecretKey,
    'SignSecretKey should match');
  AssertEqualsString('Bearer test_token', FClient.BearerToken,
    'BearerToken should match');
end;

procedure TIntellectMoneyClientTest.TestCreateInvoiceBasic;
var
  Response: string;
begin
  // Регистрируем обработчик
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-001', 'Invoice created', 100.00, 'RUB')
  );

  // Выставляем счет
  Response := FClient.CreateInvoice(
    'ORDER-001',
    100.00,
    'RUB',
    'test@example.com'
  );

  // Проверяем результат
  AssertTrue(Response <> '', 'Response should not be empty');
  AssertTrue(Response.Contains('INV-001'), 'Response should contain invoice ID');

  // Проверяем, что был сделан запрос
  AssertTrue(FMockServer.GetRequestCount > 0, 'Should have recorded a request');
end;

procedure TIntellectMoneyClientTest.TestCreateInvoiceWithAllParameters;
var
  Response: string;
  aLastReq: TRequestRecord;
begin
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-002', 'Invoice created', 250.50, 'RUB')
  );

  Response := FClient.CreateInvoice(
    'ORDER-002',
    250.50,
    'RUB',
    'user@example.com',
    'Иван Петров',
    'Оплата за товары',
    'https://shop.com/success',
    'https://shop.com/fail',
    'https://shop.com/back',
    'https://shop.com/notify',
    '2025-12-31 23:59:59',
    1,  // holdMode
    24  // holdTime
  );

  AssertTrue(Response <> '', 'Response should not be empty');
  AssertTrue(Response.Contains('INV-002'), 'Response should contain invoice ID');

  // Проверяем параметры запроса
  aLastReq := FMockServer.GetLastRequest;
  AssertTrue(aLastReq.Body.Contains('ORDER-002'), 'Request should contain order ID');
end;

procedure TIntellectMoneyClientTest.TestCreateInvoiceValidation;
var
  Response: string;
begin
  // Тест с минимальной суммой (менее 10.00 должно быть отклонено на стороне сервера)
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateErrorResponse('InvalidAmount', 'Amount must be >= 10.00')
  );

  Response := FClient.CreateInvoice(
    'ORDER-SMALL',
    5.00,
    'RUB',
    'test@example.com'
  );

  AssertTrue(Response <> '', 'Should have error response');
  AssertTrue(Response.Contains('InvalidAmount'), 'Response should contain error code');
end;

procedure TIntellectMoneyClientTest.TestGetPaymentState;
var
  PaymentState: TJSONObject;
begin
  FMockServer.RegisterHandler(
    '/getbankcardpaymentstate',
    TResponseFactory.CreatePaymentStateResponse('COMPLETED', 'Success', 100.00)
  );

  PaymentState := FClient.GetPaymentState('INV-001');

  try
    AssertTrue(Assigned(PaymentState), 'PaymentState should not be nil');
    if Assigned(PaymentState) then
    begin
      AssertTrue(PaymentState.FindPath('Result.PaymentStep') <> nil,
        'Response should contain PaymentStep');
    end;
  finally
    if Assigned(PaymentState) then
      PaymentState.Free;
  end;
end;

procedure TIntellectMoneyClientTest.TestHashCalculation;
var
  // Этот тест проверяет, что хеш вычисляется консистентно
  aReq1, aReq2, aReq3: TRequestRecord;
begin
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-003', 'OK', 100.00, 'RUB')
  );
  FMockServer.ClearRequests;

  // Первый запрос
  FClient.CreateInvoice('ORDER-003', 100.00, 'RUB', 'test@example.com');
  aReq1 := FMockServer.GetLastRequest;

  FMockServer.ClearRequests;

  // Второй запрос с тем же параметрами
  FClient.CreateInvoice('ORDER-003', 100.00, 'RUB', 'test@example.com');
  aReq2 := FMockServer.GetLastRequest;

  // Хеши должны совпадать для одинаковых параметров
  AssertTrue(aReq1.Body.Contains('hash'), 'First request should contain hash');
  AssertTrue(aReq2.Body.Contains('hash'), 'Second request should contain hash');

  FClient.EshopId:='465932';
  FClient.SecretKey:='123456';

  FClient.CreateInvoice('ORDER_ID_12345678', 10.00, 'TST', 'e.mozgovoy+1@intellectmoney.ru');
  aReq3 := FMockServer.GetLastRequest;
  AssertTrue(aReq3.Body.Contains('a84dc38cd0883e4c822ba657028fd47e'), 'Hash should valid');
end;

procedure TIntellectMoneyClientTest.TestErrorHandling;
var
  aResponse: String;
begin
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateErrorResponse('INVALID_ESHOP', 'Invalid eshop ID')
  );

  aResponse := FClient.CreateInvoice('ORDER-ERR', 100.00, 'RUB', 'test@example.com');

  AssertTrue(aResponse <> '', 'Should get error response');
  AssertTrue(aResponse.Contains('INVALID_ESHOP'), 'Response should contain error code');
end;

initialization
  RegisterTest('IntellectMoneyClient', TIntellectMoneyClientTest);

end.
