unit X_DSA.GreedyAlgorithm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  X_DSA.Utils;

type
  THashSets = specialize THashMap<UString, TObject>;
  TBroadcasts = specialize THashMap<UString, THashSets>;
  TListOfStr = specialize TList<UString>;


procedure Main;

implementation

procedure Main;
var
  set1, set2, set3, set4, set5, areas, allAreas, tempSet: THashSets;
  broadcasts: TBroadcasts;
  selects: TListOfStr;
  maxKey, key: UString;
  i: integer;
begin
  // -------------------------------------------------------------
  // 创建广播电台
  set1 := THashSets.Create;
  set1.Add('北京', nil);
  set1.Add('上海', nil);
  set1.Add('天津', nil);

  set2 := THashSets.Create;
  set2.Add('广州', nil);
  set2.Add('北京', nil);
  set2.Add('深圳', nil);

  set3 := THashSets.Create;
  set3.Add('成都', nil);
  set3.Add('上海', nil);
  set3.Add('杭州', nil);


  set4 := THashSets.Create;
  set4.Add('上海', nil);
  set4.Add('天津', nil);

  set5 := THashSets.Create;
  set5.Add('杭州', nil);
  set5.Add('大连', nil);
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
  allAreas.Add('北京', nil);
  allAreas.Add('上海', nil);
  allAreas.Add('天津', nil);
  allAreas.Add('广州', nil);
  allAreas.Add('深圳', nil);
  allAreas.Add('成都', nil);
  allAreas.Add('杭州', nil);
  allAreas.Add('大连', nil);

  // 创建List, 存放选择的电台集合
  selects := TListOfStr.Create;

  // 定义一个临时的集合，
  // 在遍历的过程中，存放遍历过程中的电台覆盖的地区和当前还没有覆盖的地区的交集
  tempSet := THashSets.Create;

  while allAreas.Count <> 0 do
    // 如果 allAreas 不为 0, 则表示还没有覆盖到所有的地区
  begin
    // 定义给 maxKey，保存在一次遍历过程中，能够覆盖最大未覆盖的地区对应的电台的key
    // 如果 maxKey 不为 nil , 则会加入到 selects
    maxKey := '';

    //遍历 broadcasts, 取出对应 key
    for key in broadcasts.Keys do
    begin
      tempSet.Clear;

      // 当前这个key能够覆盖的地区
      areas := broadcasts[key];

      // 求出 ares 和 allAreas 集合的交集, 交集赋给 tempSet
      for i := 0 to areas.Count - 1 do
      begin
        if allAreas.ContainsKey(areas.Keys.ToArray[i]) then
          tempSet.Add(areas.Keys.ToArray[i], nil);
      end;

      // 如果当前这个集合包含的未覆盖地区的数量，比 maxKey 指向的集合地区还多
      // 就需要重置 maxKey
      // tempSet.Count > broadcasts.Items[key].Count
      // 体现出贪心算法的特点,每次都选择最优的
      if (tempSet.Count > 0) and
        ((maxKey = '') or (tempSet.Count > broadcasts[maxKey].Count)) then
        maxKey := key;
    end;

    // maxKey <> nil, 就应该将 maxKey 加入 selects
    // 并将 maxKey 指向的广播电台覆盖的地区，从 allAreas 去掉
    if maxKey <> '' then
    begin
      selects.Add(maxKey);

      for i := 0 to broadcasts[maxKey].Count - 1 do
      begin
        if allAreas.ContainsKey(broadcasts[maxKey].Keys.ToArray[i]) then
          allAreas.Remove(broadcasts[maxKey].Keys.ToArray[i]);
      end;
    end;
  end;

  for i := 0 to selects.Count - 1 do
    WriteLn(selects.Items[i]);
end;

end.