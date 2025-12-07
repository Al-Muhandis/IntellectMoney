program gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, IntellectMoneyClientTests, IntellectMoneyIntegrationTests,
  IntellectMoneyParameterTests, intellectmoney_api, intellectmoney_merchant, intellectmoneymerchanttests
  ;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

