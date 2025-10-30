// ============================================================================
// IntellectMoneyIntegrationTests.pas - Integration Tests
// ============================================================================

unit IntellectMoneyIntegrationTests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, IntellectMoney, IntellectMoneyTests, MockServer, fpJSON
  ;

type
  TIntellectMoneyIntegrationTest = class(TIntellectMoneyTestCase)
  published
    procedure TestCreateAndCheckPaymentFlow;
    procedure TestMultipleInvoices;
    procedure TestBankCardPaymentFlow;
    procedure TestConcurrentRequests;
  end;

implementation

{ TIntellectMoneyIntegrationTest }

procedure TIntellectMoneyIntegrationTest.TestCreateAndCheckPaymentFlow;
var
  InvoiceId: string;
  PaymentState: TJSONObject;
begin
  // Этап 1: Создаем счет
  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-INT-001', 'Invoice created', 500.00, 'RUB')
  );

  InvoiceId := FClient.CreateInvoice(
    'ORDER-INT-001',
    500.00,
    'RUB',
    'customer@example.com',
    'Test Customer',
    'Integration Test Payment'
  );

  AssertTrue(InvoiceId <> '', 'Invoice should be created');
  AssertTrue(InvoiceId.Contains('INV'), 'Invoice ID should contain "INV"');

  // Этап 2: Проверяем статус платежа
  FMockServer.RegisterHandler(
    '/getbankcardpaymentstate',
    TResponseFactory.CreatePaymentStateResponse('PENDING', 'Success', 500.00)
  );

  PaymentState := FClient.GetPaymentState('INV-INT-001');

  try
    AssertTrue(Assigned(PaymentState), 'Payment state should be retrieved');
  finally
    if Assigned(PaymentState) then
      PaymentState.Free;
  end;

  // Проверяем количество запросов
  AssertTrue(FMockServer.GetRequestCount >= 2, 'Should have at least 2 requests');
end;

procedure TIntellectMoneyIntegrationTest.TestMultipleInvoices;
var
  i: Integer;
  InvoiceIds: array of string = ();
begin
  SetLength(InvoiceIds, 3);

  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-MULTI', 'Invoice created', 100.00, 'RUB')
  );

  FMockServer.ClearRequests;

  // Создаем несколько счетов
  for i := 0 to 2 do
  begin
    InvoiceIds[i] := FClient.CreateInvoice(
      'ORDER-MULTI-' + IntToStr(i + 1),
      100.00,
      'RUB',
      'customer' + IntToStr(i + 1) + '@example.com'
    );
  end;

  // Проверяем, что все счета созданы
  for i := 0 to 2 do
    AssertTrue(InvoiceIds[i] <> '', 'Invoice ' + IntToStr(i + 1) + ' should be created');

  // Проверяем количество запросов
  AssertTrue(FMockServer.GetRequestCount = 3, 'Should have exactly 3 requests');
end;

procedure TIntellectMoneyIntegrationTest.TestBankCardPaymentFlow;
var
  PaymentResult: TJSONObject;
begin
  FMockServer.RegisterHandler(
    '/bankcardpayment',
    TResponseFactory.CreateBankCardPaymentResponse('Success', 'Payment processed', True)
  );

  PaymentResult := FClient.BankCardPayment(
    'INV-CARD-001',
    '4111111111111111',
    'IVAN PETROV',
    '12',
    '2025',
    '123',
    'https://example.com/return',
    '192.168.1.1'
  );

  try
    AssertTrue(Assigned(PaymentResult), 'Payment result should be retrieved');
  finally
    if Assigned(PaymentResult) then
      PaymentResult.Free;
  end;
end;

procedure TIntellectMoneyIntegrationTest.TestConcurrentRequests;
var
  i: Integer;
  Responses: array of string = ();
begin
  SetLength(Responses, 5);

  FMockServer.RegisterHandler(
    '/createinvoice',
    TResponseFactory.CreateInvoiceResponse('INV-CONC', 'Invoice created', 100.00, 'RUB')
  );

  FMockServer.ClearRequests;

  // Создаем несколько счетов последовательно
  for i := 0 to 4 do
  begin
    Responses[i] := FClient.CreateInvoice(
      'ORDER-CONC-' + IntToStr(i + 1),
      100.00,
      'RUB',
      'user' + IntToStr(i + 1) + '@example.com'
    );
    AssertTrue(Responses[i] <> '', 'Request ' + IntToStr(i + 1) + ' should succeed');
  end;

  // Проверяем, что все запросы были выполнены
  AssertTrue(FMockServer.GetRequestCount = 5, 'Should have exactly 5 requests');
end;

initialization
  RegisterTest('IntellectMoneyIntegration', TIntellectMoneyIntegrationTest);

end.
