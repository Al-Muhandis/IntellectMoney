// ============================================================================
// IntellectMoneyTests.pas - Base Test Case Class for IntellectMoney Tests
// ============================================================================

unit IntellectMoneyTests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, IntellectMoney, MockServer;

type
  TIntellectMoneyTestCase = class(TTestCase)
  protected
    FClient: TMockIntellectMoneyClient;
    FMockServer: TMockServer;
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure AssertEqualsString(const AExpected, AActual, AMsg: string);
    procedure AssertTrue(ACondition: Boolean; const AMsg: string);
    procedure AssertFalse(ACondition: Boolean; const AMsg: string);
  end;

implementation

{ TIntellectMoneyTestCase }

procedure TIntellectMoneyTestCase.SetUp;
begin
  inherited SetUp;

  // Инициализируем mock сервер
  FMockServer := TMockServer.Create(8888);
  FMockServer.ServerStart;
  Sleep(500); // Даем серверу время на инициализацию

  // Инициализируем клиент
  FClient := TMockIntellectMoneyClient.Create(
    '12345',
    'test_secret_key',
    'test_sign_secret_key',
    'Bearer test_token'
  );
  FClient.ApiUrl:='http://127.0.0.1:8888';
end;

procedure TIntellectMoneyTestCase.TearDown;
begin
  FClient.Free;

  if Assigned(FMockServer) then
  begin
    FMockServer.ServerStop;
    FMockServer.Free;
  end;

  inherited TearDown;
end;

procedure TIntellectMoneyTestCase.AssertEqualsString(const AExpected,
  AActual, AMsg: string);
begin
  if AExpected <> AActual then
    Fail(Format('%s: Expected "%s", got "%s"', [AMsg, AExpected, AActual]));
end;

procedure TIntellectMoneyTestCase.AssertTrue(ACondition: Boolean;
  const AMsg: string);
begin
  if not ACondition then
    Fail(AMsg);
end;

procedure TIntellectMoneyTestCase.AssertFalse(ACondition: Boolean;
  const AMsg: string);
begin
  if ACondition then
    Fail(AMsg);
end;

end.
