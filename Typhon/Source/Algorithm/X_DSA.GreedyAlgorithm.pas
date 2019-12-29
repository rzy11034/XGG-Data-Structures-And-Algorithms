unit X_DSA.GreedyAlgorithm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type
  THashSets = specialize THashSets<UnicodeString, TObject>;
  TBroadcasts = specialize THashSets<UnicodeString, THashSets>;
  TListOfStr = specialize TList<UnicodeString>;


procedure Main;

implementation

procedure Main;
var
  set1, set2, set3, set4, set5: THashSets;
  broadcasts: TBroadcasts;
  allAreas, tempSet: THashSets;
  selects: TListOfStr;
  maxKey: UnicodeString;
begin
  // -------------------------------------------------------------
  // 创建广播电台
  set1 := THashSets.Create;
  set1.add('北京', nil);
  set1.add('上海', nil);
  set1.add('天津', nil);

  set2 := THashSets.Create;
  set2.add('广州', nil);
  set2.add('北京', nil);
  set2.add('深圳', nil);

  set3 := THashSets.Create;
  set3.add('成都', nil);
  set3.add('上海', nil);
  set3.add('杭州', nil);


  set4 := THashSets.Create;
  set4.add('上海', nil);
  set4.add('天津', nil);

  set5 := THashSets.Create;
  set5.add('杭州', nil);
  set5.add('大连', nil);
  //------------------------------------------------------------

  // 将各个电台放入到broadcasts
  broadcasts := TBroadcasts.Create;
  broadcasts.Add('K1', set1);
  broadcasts.Add('K2', set2);
  broadcasts.Add('K3', set3);
  broadcasts.Add('K4', set4);
  broadcasts.Add('K5', set5);

  // allAreas 存放所有的地区
  allAreas := THashSets.Create;
  allAreas.add('北京', nil);
  allAreas.add('上海', nil);
  allAreas.add('天津', nil);
  allAreas.add('广州', nil);
  allAreas.add('深圳', nil);
  allAreas.add('成都', nil);
  allAreas.add('杭州', nil);
  allAreas.add('大连', nil);

  // 创建List, 存放选择的电台集合
  selects := TListOfStr.Create;

  // 定义一个临时的集合，
  // 在遍历的过程中，存放遍历过程中的电台覆盖的地区和当前还没有覆盖的地区的交集
  tempSet := THashSets.Create;

  // 定义给 maxKey，保存在一次遍历过程中，能够覆盖最大未覆盖的地区对应的电台的key
	// 如果 maxKey 不为 nil , 则会加入到 selects
  maxKey := '';
end;

end.
