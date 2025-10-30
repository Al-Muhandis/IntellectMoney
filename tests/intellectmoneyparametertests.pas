// ============================================================================
// IntellectMoneyParameterTests.pas - Parameter Validation Tests
// ============================================================================

unit IntellectMoneyParameterTests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, IntellectMoney, IntellectMoneyTests, MockServer
  ;

type
  TIntellectMoneyParameterTest = class(TIntellectMoneyTestCase)
  published
    procedure TestOrderIdMaxLength;
    procedure TestAmountFormatting;
    procedure TestCurrencyValidation;
    procedure TestEmailValidation;
    procedure TestExpireDateFormat;
    procedure TestAllParametersPresent;
  end;

implementation

{ TIntellectMoneyParameterTest }

procedure TIntellectMoneyParameterTest.TestOrderIdMaxLength;
var
  LongOrderId: string;
  Response: string;
  aLastReq: TRequestRecord;
begin
  // Заказ ID может быть до 50 символов
  LongOrderId := StringOfChar('A', 50);

  FMockServer.RegisterHandler(
    '/createInvoice',
    TResponseFactory.CreateInvoiceResponse('INV-PARAM-001', 'OK', 100.00, 'RUB')
  );

  Response := FClient.CreateInvoice(
    LongOrderId,
    100.00,
    'RUB',
    'test@example.com'
  );

  AssertTrue(Response <> '', 'Should accept order ID with 50 chars');

  aLastReq := FMockServer.GetLastRequest;
  AssertTrue(aLastReq.Body.Contains(LongOrderId), 'Request should contain full order ID');
end;

procedure TIntellectMoneyParameterTest.TestAmountFormatting;
var
  aLastReq: TRequestRecord;
begin
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-PARAM-002', 'OK', 123.45, 'RUB')
  );

  FClient.CreateInvoice(
    'ORDER-AMOUNT',
    123.456,  // Должно быть отформатировано до 123.46
    'RUB',
    'test@example.com'
  );

  aLastReq := FMockServer.GetLastRequest;
  AssertTrue(aLastReq.Body.Contains('123.46'), 'Amount should be formatted with 2 decimal places');
end;

procedure TIntellectMoneyParameterTest.TestCurrencyValidation;
var
  Response: string;
  aLastReq: TRequestRecord;
begin
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-PARAM-003', 'OK', 100.00, 'USD')
  );

  Response := FClient.CreateInvoice(
    'ORDER-USD',
    100.00,
    'USD',
    'test@example.com'
  );

  AssertTrue(Response <> '', 'Should accept USD currency');

  aLastReq := FMockServer.GetLastRequest;
  AssertTrue(aLastReq.Body.Contains('USD'), 'Request should contain currency code');
end;

procedure TIntellectMoneyParameterTest.TestEmailValidation;
var
  Response: string;
  aLastReq: TRequestRecord;
begin
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-PARAM-004', 'OK', 100.00, 'RUB')
  );

  Response := FClient.CreateInvoice(
    'ORDER-EMAIL',
    100.00,
    'RUB',
    'very.long.email.address.with.many.parts@subdomain.example.com'
  );

  AssertTrue(Response <> '', 'Should accept long email addresses');

  aLastReq := FMockServer.GetLastRequest;
  AssertTrue(aLastReq.Body.Contains('very.long.email'),
    'Request should contain email address');
end;

procedure TIntellectMoneyParameterTest.TestExpireDateFormat;
var
  Response: string;
  aLastReq: TRequestRecord;
begin
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-PARAM-005', 'OK', 100.00, 'RUB')
  );

  Response := FClient.CreateInvoice(
    'ORDER-DATE',
    100.00,
    'RUB',
    'test@example.com',
    'Test User',
    'Test',
    '',
    '',
    '',
    '',
    '2025-12-31 23:59:59'  // Формат: yyyy-MM-dd HH:mm:ss
  );

  AssertTrue(Response <> '', 'Should accept properly formatted expiry date');

  aLastReq := FMockServer.GetLastRequest;
  AssertTrue(aLastReq.Body.Contains('2025-12-31'), 'Request should contain expiry date');
end;

procedure TIntellectMoneyParameterTest.TestAllParametersPresent;
var
  aLastReq: TRequestRecord;
begin
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-PARAM-006', 'OK', 250.00, 'RUB')
  );

  FMockServer.ClearRequests;

  FClient.CreateInvoice(
    'ORDER-FULL',
    250.00,
    'RUB',
    'customer@example.com',
    'Иван Петров',
    'Оплата за подписку на 3 месяца',
    'https://shop.com/success',
    'https://shop.com/fail',
    'https://shop.com/back',
    'https://shop.com/notify',
    '2025-12-31 23:59:59',
    1,  // holdMode
    48  // holdTime
  );

  aLastReq := FMockServer.GetLastRequest;

  // Проверяем наличие всех параметров
  AssertTrue(aLastReq.Body.Contains('eshopId'), 'Should contain eshopId');
  AssertTrue(aLastReq.Body.Contains('ORDER-FULL'), 'Should contain orderId');
  AssertTrue(aLastReq.Body.Contains('customer@example.com'), 'Should contain email');
  AssertTrue(aLastReq.Body.Contains('Иван Петров'), 'Should contain userName');
  AssertTrue(aLastReq.Body.Contains('подписку'), 'Should contain serviceName');
  AssertTrue(aLastReq.Body.Contains('hash'), 'Should contain hash');
end;

initialization
  RegisterTest('IntellectMoneyParameters', TIntellectMoneyParameterTest);

end.
