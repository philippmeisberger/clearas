{ *********************************************************************** }
{                                                                         }
{ PM Code Works Report Bug Unit v1.0                                      }
{                                                                         }
{ Copyright (c) 2011-2019 Philipp Meisberger (PM Code Works)              }
{                                                                         }
{ *********************************************************************** }

unit PMCW.Dialogs.ReportBug;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms, Vcl.Controls,
  Vcl.StdCtrls, System.UITypes, System.Net.HttpClient, Vcl.Dialogs, PMCW.SysUtils,
  PMCW.LanguageFile;

type
  /// <summary>
  ///   Thread for sending a bug report.
  /// </summary>
  TReportBugThread = class(TThread)
  private
    FReport: string;
    FHttpClient: THTTPClient;
  protected
    procedure Execute(); override;
  public
    /// <summary>
    ///   Constructor for creating a <c>TReportBugThread</c> instance.
    /// </summary>
    /// <param name="AReport">
    ///   The report to send.
    /// </param>
    constructor Create(const AReport: string);

    /// <summary>
    ///   Destructor for destroying a <c>TReportBugThread</c> instance.
    /// </summary>
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Dialog for sending a bug report.
  /// </summary>
  TReportBugDialog = class(TCommonDialog)
  private
    FForm: TForm;
    FReport: TMemo;
    FSubmit,
    FCancel: TButton;
    FLanguageFile: TLanguageFile;
    function GetTitle(): string;
    procedure SetTitle(const ATitle: string);
    function GetReport(): TStrings;
    procedure SubmitClick(Sender: TObject);
  public
    /// <summary>
    ///   Constructor for creating a <c>TReportBugDialog</c> instance.
    /// </summary>
    /// <param name="AOwner">
    ///   The owner.
    /// </param>
    constructor Create(AOwner: TComponent); overload; override;

    /// <summary>
    ///   Constructor for creating a <c>TUpdateDialog</c> instance.
    /// </summary>
    /// <param name="AOwner">
    ///   The owner.
    /// </param>
    /// <param name="ALanguageFile">
    ///   The user interface translation file to use.
    /// </param>
    constructor Create(AOwner: TComponent; ALanguageFile: TLanguageFile); reintroduce; overload;

    /// <summary>
    ///   Destructor for destroying a <c>TReportBugDialog</c> instance.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Executes the dialog.
    /// </summary>
    /// <param name="AParentHwnd">
    ///   The parent window.
    /// </param>
    /// <returns>
    ///   <c>True</c> if upload was sucessful or <c>False</c> otherwise.
    /// </returns>
    function Execute(AParentHwnd: HWND): Boolean; override;

    /// <summary>
    ///   The specific user interface translation file to use.
    /// </summary>
    property LanguageFile: TLanguageFile read FLanguageFile write FLanguageFile;

    /// <summary>
    ///   Gets or presets the report.
    /// </summary>
    property Report: TStrings read GetReport;

    /// <summary>
    ///   Gets or sets the title to use in the dialog caption.
    /// </summary>
    property Title: string read GetTitle write SetTitle;
  end;

implementation

{ TReportBugThread }

constructor TReportBugThread.Create(const AReport: string);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FReport := AReport;

  // Initialize HTTP connection
  FHttpClient := THttpClient.Create;

  with FHttpClient do
  begin
    UserAgent := 'ReportBug/1.0 (PM Code Works Report Bug Utility)';
    CustomHeaders['Connection'] := 'close';
  end;  //of begin
end;

destructor TReportBugThread.Destroy;
begin
  FreeAndNil(FHttpClient);
  inherited Destroy;
end;

procedure TReportBugThread.Execute;
var
  Content: TStringList;
  Response: IHTTPResponse;

begin
  Content := TStringList.Create;

  try
    Content.Append('app='+ Application.Title);
    Content.Append('message='+ FReport);
    Response := FHttpClient.Post(URL_BASE +'report.php', Content);

    if (Response.StatusCode <> 200) then
      raise ENetHTTPException.Create(Response.StatusText);

  finally
    FreeAndNil(Content);
  end;  //of try
end;


{ TReportBugDialog }

constructor TReportBugDialog.Create(AOwner: TComponent);
const
  cMarginTop = 5;
  cButtonHeight = 25;

begin
  inherited Create(AOwner);
  FForm := TForm.Create(Self);

  with FForm do
  begin
    BorderIcons := [biSystemMenu];
    BorderStyle := bsSizeable;
    Caption := 'Report bug';
    ClientHeight := 180;
    ClientWidth := 330;
    Constraints.MinHeight := 160;
    Constraints.MinWidth := 190;
    Position := poScreenCenter;
  end;  //of with

  FReport := TMemo.Create(FForm);

  with FReport do
  begin
    Parent := FForm;
    Top := 2 * cMarginTop;
    Left := Top;
    Width := FForm.Width - (2 * Left);
    Height := FForm.Height - Top - cButtonHeight - (2 * cMarginTop);
    Anchors := [akLeft, akTop, akRight, akBottom];
    ScrollBars := ssBoth;
  end;

  FSubmit := TButton.Create(FForm);

  with FSubmit do
  begin
    Parent := FForm;
    Width := 3 * cButtonHeight;
    Height := cButtonHeight;
    Left := FReport.Left + FReport.Width - Width;
    Top := FReport.Top + FReport.Height + cMarginTop;
    Anchors := [akRight, akBottom];
    Caption := 'Submit';
    Default := True;
    ModalResult := mrOk;
    OnClick := SubmitClick;
  end;  //of with

  FCancel := TButton.Create(FForm);

  with FCancel do
  begin
    Parent := FForm;
    Left := FReport.Left;
    Top := FSubmit.Top;
    Width := FSubmit.Width;
    Height := FSubmit.Height;
    Anchors := [akLeft, akBottom];
    Cancel := True;
    Caption := 'Cancel';
    ModalResult := mrCancel;
  end;  //of with
end;

constructor TReportBugDialog.Create(AOwner: TComponent;
  ALanguageFile: TLanguageFile);
begin
  Create(AOwner);
  Assert(Assigned(ALanguageFile), 'ALanguageFile is not assigned!');
  FLanguageFile := ALanguageFile;
  FCancel.Caption := FLanguageFile[LID_CANCEL];
  FSubmit.Caption := FLanguageFile[LID_REPORT_SUBMIT];
  FForm.Caption := FLanguageFile[LID_REPORT_BUG];
end;

destructor TReportBugDialog.Destroy;
begin
  FreeAndNil(FReport);
  FreeAndNil(FSubmit);
  FreeAndNil(FCancel);
  FreeAndNil(FForm);
  inherited Destroy;
end;

function TReportBugDialog.Execute(AParentHwnd: HWND): Boolean;
begin
  Result := (FForm.ShowModal() = mrOk);

  if Result then
    TReportBugThread.Create(FReport.Lines.Text);
end;

function TReportBugDialog.GetReport(): TStrings;
begin
  Result := FReport.Lines;
end;

function TReportBugDialog.GetTitle(): string;
begin
  Result := FForm.Caption;
end;

procedure TReportBugDialog.SetTitle(const ATitle: string);
begin
  FForm.Caption := ATitle;
end;

procedure TReportBugDialog.SubmitClick(Sender: TObject);
begin
  if (FReport.Lines.Text = '') then
    FForm.ModalResult := mrNone;
end;

end.
