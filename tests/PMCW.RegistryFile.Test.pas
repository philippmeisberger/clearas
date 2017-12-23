unit PMCW.RegistryFile.Test;

interface

uses
  TestFramework, Winapi.Windows, System.SysUtils, System.Classes, Registry,
  PMCW.RegistryFile;

type
  TRegistryFileTest = class(TTestCase)
  strict private
    FRegistryFile: TRegistryFile;
  public
    const
      cSection = 'Section';
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBinaryData;
    procedure TestBool;
    procedure TestBytes;
    procedure TestDate;
    procedure TestDateTime;
    procedure TestExpandString;
    procedure TestExport;
    procedure TestFloat;
    procedure TestInteger;
    procedure TestString;
    procedure TestTime;
  end;

implementation

procedure TRegistryFileTest.SetUp;
begin
  FRegistryFile := TRegistryFile.Create(ClassName + TRegistryFile.FileExtension);
end;

procedure TRegistryFileTest.TearDown;
begin
  FreeAndNil(FRegistryFile);
end;

procedure TRegistryFileTest.TestBinaryData;
const
  cIdentifier = 'Binary';

var
  Expected, Value: Int64;

begin
  Expected := 123456789123456789;
  FRegistryFile.WriteBinaryData(cSection, cIdentifier, Expected, SizeOf(Int64));
  CheckEquals(SizeOf(Int64), FRegistryFile.ReadBinaryData(cSection, cIdentifier, Value, SizeOf(Int64)), 'Return value differs from value written');
  CheckEquals(Expected, Value);
  FRegistryFile.DeleteKey(cSection, cIdentifier);
  CheckEquals(0, FRegistryFile.ReadBinaryData(cSection, cIdentifier, Value, SizeOf(Int64)), 'When trying to delete a non-existing key the default value should be used');
end;

procedure TRegistryFileTest.TestBytes;
const
  cIdentifier = 'Bytes';

var
  Expected, Value: TBytes;

begin
  Expected := BytesOf('Bytes');
  FRegistryFile.WriteBytes(cSection, cIdentifier, Expected);
  Value := FRegistryFile.ReadBytes(cSection, cIdentifier);
  CheckEqualsMem(Pointer(Expected), Pointer(Value), Length(Value), 'Return value differs from value written');
  FRegistryFile.DeleteKey(cSection, cIdentifier);
  Value := FRegistryFile.ReadBytes(cSection, cIdentifier);
  CheckEquals(0, Length(Value), 'When trying to delete a non-existing key the default value should be used');
end;

procedure TRegistryFileTest.TestBool;
const
  cTrue  = 'True';
  cFalse = 'False';

begin
  FRegistryFile.WriteBool(cSection, cFalse, False);
  FRegistryFile.WriteBool(cSection, cTrue, True);
  CheckFalse(FRegistryFile.ReadBool(cSection, cFalse, True), 'Return value differs from value written');
  CheckTrue(FRegistryFile.ReadBool(cSection, cTrue, False), 'Return value differs from value written');
  FRegistryFile.DeleteKey(cSection, cTrue);
  CheckFalse(FRegistryFile.ReadBool(cSection, cTrue, False), 'When trying to delete a non-existing key the default value should be used');
end;

procedure TRegistryFileTest.TestDate;
const
  cIdentifier = 'Date';
  cDefault    = 1;

var
  Value: TDate;

begin
  Value := Date();
  FRegistryFile.WriteDate(cSection, cIdentifier, Value);
  CheckEquals(Value, FRegistryFile.ReadDate(cSection, cIdentifier, cDefault), 'Return value differs from value written');
  FRegistryFile.DeleteKey(cSection, cIdentifier);
  CheckEquals(cDefault, FRegistryFile.ReadDate(cSection, cIdentifier, cDefault), 'When trying to delete a non-existing key the default value should be used');
end;

procedure TRegistryFileTest.TestDateTime;
const
  cIdentifier = 'DateTime';
  cDefault    = 1;

var
  Value: TDateTime;

begin
  Value := Now();
  FRegistryFile.WriteDate(cSection, cIdentifier, Value);
  CheckEquals(Value, FRegistryFile.ReadDateTime(cSection, cIdentifier, cDefault), 'Return value differs from value written');
  FRegistryFile.DeleteKey(cSection, cIdentifier);
  CheckEquals(cDefault, FRegistryFile.ReadDate(cSection, cIdentifier, cDefault), 'When trying to delete a non-existing key the default value should be used');
end;

procedure TRegistryFileTest.TestExpandString;
const
  cIdentifier = 'ExpandString';
  cDefault    = 'default';

var
  Value: string;

begin
  Value := '%WINDOWS%\explorer.exe';
  FRegistryFile.WriteExpandString(cSection, cIdentifier, Value);
  CheckEqualsString(Value, FRegistryFile.ReadExpandString(cSection, cIdentifier, cDefault), 'Return value differs from value written');
  FRegistryFile.DeleteKey(cSection, cIdentifier);
  CheckEquals(cDefault, FRegistryFile.ReadExpandString(cSection, cIdentifier, cDefault), 'When trying to delete a non-existing key the default value should be used');
end;

procedure TRegistryFileTest.TestExport;
const
  cString       = 'String';
  cExpandString = 'ExpandString';
  cInteger      = 'Integer';
  cBinary       = 'Binary';
  cKey          = 'Software\RegistryFileTest';
  cSubkey       = 'Software\RegistryFileTest\Subkey';

var
  Reg: TRegistry;
  StringValue, ExpandStringValue: string;
  IntegerValue: Integer;
  BinaryValue: TBytes;
  Section: TStrings;

  procedure TestValuesExported(const AKey: string);
  begin
    CheckEqualsString(StringValue, FRegistryFile.ReadString(AKey, cString, ''), 'String was not exported correctly');
    CheckEquals(ExpandStringValue, FRegistryFile.ReadExpandString(AKey, cExpandString, ''), 'ExpandString was not exported correctly');
    CheckEquals(IntegerValue, FRegistryFile.ReadInteger(AKey, cInteger, 0), 'Integer was not exported correctly');
    CheckEqualsMem(Pointer(BinaryValue), Pointer(FRegistryFile.ReadBytes(AKey, cBinary)), Length(BinaryValue), 'Bytes were not exported correctly');
  end;

begin
  // Create test key
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  Section := TStringList.Create;

  try
    Reg.RootKey := HKEY_CURRENT_USER;

    // Create key
    CheckTrue(Reg.OpenKey(cKey, True), Reg.LastErrorMsg);

    StringValue := 'StringValue';
    Reg.WriteString(cString, StringValue);

    ExpandStringValue := '%WINDOWS%\explorer.exe';
    Reg.WriteExpandString(cExpandString, ExpandStringValue);

    IntegerValue := 1234567890;
    Reg.WriteInteger(cInteger, IntegerValue);

    BinaryValue := BytesOf('Bytes');
    Reg.WriteBinaryData(cBinary, BinaryValue[0], Length(BinaryValue));
    Reg.CloseKey();

    // Create subkey
    CheckTrue(Reg.OpenKey(cSubkey, True), Reg.LastErrorMsg);
    Reg.WriteString(cString, StringValue);
    Reg.WriteExpandString(cExpandString, ExpandStringValue);
    Reg.WriteInteger(cInteger, IntegerValue);
    Reg.WriteBinaryData(cBinary, BinaryValue[0], Length(BinaryValue));

    // Export key non-recursive
    FRegistryFile.ExportKey(HKEY_CURRENT_USER, cKey, False);
    TestValuesExported('HKEY_CURRENT_USER\'+ cKey);
    CheckEqualsString('', FRegistryFile.ReadString('HKEY_CURRENT_USER\'+ cSubkey, cString, ''), 'Subkey must not have been exported since Recursive was False');
    FRegistryFile.Clear();

    // Export key recursive
    FRegistryFile.ExportKey(HKEY_CURRENT_USER, cKey, True);
    TestValuesExported('HKEY_CURRENT_USER\'+ cKey);
    TestValuesExported('HKEY_CURRENT_USER\'+ cSubkey);
    FRegistryFile.Clear();

    // Export single value
    FRegistryFile.ExportValue(HKEY_CURRENT_USER, cKey, cString);
    CheckEqualsString(StringValue, FRegistryFile.ReadString('HKEY_CURRENT_USER\'+ cKey, cString, ''));
    FRegistryFile.ReadSection('HKEY_CURRENT_USER\'+ cKey, Section);
    CheckEquals(1, Section.Count, 'Section must exactly contain 1 item');

  finally
    Section.Free;
    Reg.CloseKey();
    Reg.DeleteKey(cKey);
    Reg.Free;
  end;
end;

procedure TRegistryFileTest.TestFloat;
const
  cIdentifier = 'Float';
  cDefault    = -1.0;

var
  Value: Double;

begin
  Value := Pi;
  FRegistryFile.WriteFloat(cSection, cIdentifier, Value);
  CheckEquals(Value, FRegistryFile.ReadFloat(cSection, cIdentifier, cDefault), 'Return value differs from value written');
  FRegistryFile.DeleteKey(cSection, cIdentifier);
  CheckEquals(cDefault, FRegistryFile.ReadFloat(cSection, cIdentifier, cDefault), 'When trying to delete a non-existing key the default value should be used');
end;

procedure TRegistryFileTest.TestInteger;
const
  cIdentifier = 'Integer';
  cDefault    = -1;

var
  Value: Integer;

begin
  Value := MAXLONG;
  FRegistryFile.WriteInteger(cSection, cIdentifier, Value);
  CheckEquals(Value, FRegistryFile.ReadInteger(cSection, cIdentifier, cDefault), 'Return value differs from value written');
  FRegistryFile.DeleteKey(cSection, cIdentifier);
  CheckEquals(cDefault, FRegistryFile.ReadDate(cSection, cIdentifier, cDefault), 'When trying to delete a non-existing key the default value should be used');
end;

procedure TRegistryFileTest.TestString;
const
  cIdentifier = 'String';
  cDefault    = 'default';

var
  Value: string;

begin
  Value := 'Test';
  FRegistryFile.WriteString(cSection, cIdentifier, Value);
  CheckEqualsString(Value, FRegistryFile.ReadString(cSection, cIdentifier, cDefault), 'Return value differs from value written');
  FRegistryFile.DeleteKey(cSection, cIdentifier);
  CheckEquals(cDefault, FRegistryFile.ReadString(cSection, cIdentifier, cDefault), 'When trying to delete a non-existing key the default value should be used');
end;

procedure TRegistryFileTest.TestTime;
const
  cIdentifier = 'Time';
  cDefault    = 1;

var
  Value: TTime;

begin
  Value := Time();
  FRegistryFile.WriteTime(cSection, cIdentifier, Value);
  CheckEquals(Value, FRegistryFile.ReadTime(cSection, cIdentifier, cDefault), 'Return value differs from value written');
  FRegistryFile.DeleteKey(cSection, cIdentifier);
  CheckEquals(cDefault, FRegistryFile.ReadDate(cSection, cIdentifier, cDefault), 'When trying to delete a non-existing key the default value should be used');
end;

initialization
  RegisterTest(TRegistryFileTest.Suite);
end.

