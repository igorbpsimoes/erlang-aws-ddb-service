%%% Copyright (C) 2013  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
%%% MA  02110-1301  USA

-module(gpb_compile_descr).

-export([encode_defs_to_descriptors/2, encode_defs_to_descriptors/3]).

-export_type([opts/0, opt/0]).

-include_lib("eunit/include/eunit.hrl").

-include("gpb_descriptor.hrl").
-include("../include/gpb.hrl").

-define(ff(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

-type opts() :: [opt()].
-type opt() :: boolean_opt(use_packages)
             | term().
-type boolean_opt(Opt) :: Opt | {Opt, boolean()}.

-record(to_defs_env, {pseudo_msg_names,
                      msg_options,
                      enum_options,
                      ext_origins, % extended location -> extensions
                      ext_msgs, % extended msg -> fields
                      ext_ranges, % extended msg -> ranges
                      reserved_nums,
                      reserved_names,
                      name_adjuster}).

%% @equiv encode_defs_to_descriptors(undefined, Defs, Opts)
-spec encode_defs_to_descriptors(gpb_defs:defs(), opts()) -> Res when
      Res :: {EncodedFileDescriptorSet::binary(),
              [{FileName, EncodedFileDescriptorProto::binary()}]},
      FileName :: string() % base name sans the .proto file name extension
                | undefined.
encode_defs_to_descriptors(Defs, Opts) ->
    encode_defs_to_descriptors(undefined, Defs, Opts).

%% @doc Turn the definitions to a 'FileDescriptorSet' and encode it.
%% Return also the constituent protos, ie the top-level proto and any
%% imported proto definitions.
%%
%% The file name of the top-level proto is fetched from the first
%% `file' element `Defs', if present. If it is omitted, the `DefaultName'
%% is used.
-spec encode_defs_to_descriptors(FileName, gpb_defs:defs(), opts()) -> Res when
      Res :: {EncodedFileDescriptorSet::binary(),
              [{FileName, EncodedFileDescriptorProto::binary()}]},
      FileName :: string() % base name sans the .proto file name extension
                | undefined.
encode_defs_to_descriptors(DefaultName, Defs, Opts) ->
    %% In case this function is called directly (ie not from gpb_compile)
    %% with some defs:
    {ok, Defs1} = gpb_defs:convert_defs_to_latest_version(Defs),
    Defs2 = synthesize_proto3_optional_oneofs(Defs1),
    %% Encode the list of files as a #'FileDescriptorSet'{}.
    %% Encode also for each constituent proto, ie the top level and each
    %% imported proto separately, as #'FileDescriptorProto'{}.
    PDefses = partition_protos(Defs2, DefaultName, [], []),
    {FdSet, Infos} = partitioned_defs_to_file_descr_set(PDefses, Opts),
    Bin = gpb_descriptor:encode_msg(FdSet, [verify]),
    PBins = [{FileNameSansExt, gpb_descriptor:encode_msg(FdProto, [verify])}
             || {FileNameSansExt, FdProto} <- Infos],
    {Bin, PBins}.

synthesize_proto3_optional_oneofs(Defs) ->
    P3Msgs = proplists:get_value(proto3_msgs, Defs, []),
    [case Item of
         {{msg,MsgName}, Fields} ->
             case lists:member(MsgName, P3Msgs) of
                 true ->
                     Fields1 = synthesize_proto3_optional_oneofs_fields(Fields),
                     {{msg,MsgName}, Fields1};
                 false ->
                     Item
             end;
         _ ->
             Item
     end
     || Item <- Defs].

synthesize_proto3_optional_oneofs_fields(Fields) ->
    %% Change all fields with occurrence=optional
    %% to oneof field containing only that single field.
    %% Invent a unique name for the new oneof field.
    %%
    %% Change the inner oneof field to have occurrence=defaulty,
    %% this will indicates that it is a synthetic oneof (oneof fields
    %% normally have occurrence=optional)
    FNames0 = collect_field_names(Fields, []),
    {Fields1, _FNames} =
        lists:mapfoldl(
          fun(#?gpb_field{occurrence=optional}=Field, FNames) ->
                  #?gpb_field{name=FName, rnum=RNum} = Field,
                  NewFName = synthesize_unique_name(FName, FNames),
                  DField = Field#?gpb_field{occurrence=defaulty},
                  Oneof = #gpb_oneof{name=NewFName, rnum=RNum, fields=[DField]},
                  {Oneof, [NewFName | FNames]};
             (OtherField, FNames) ->
                  {OtherField, FNames}
          end,
          FNames0,
          Fields),
    Fields1.

collect_field_names(Fields, InitAcc) ->
    lists:foldl(
      fun(#?gpb_field{name=FName}, Acc) ->
              [FName | Acc];
         (#gpb_oneof{name=FName, fields=OFields}, Acc) ->
              collect_field_names(OFields, [FName | Acc])
      end,
      InitAcc,
      Fields).

synthesize_unique_name(Name, Names) ->
    Candidate = list_to_atom("_" ++ atom_to_list(Name)),
    case lists:member(Name, Names) of
        true  -> synthesize_unique_name(Candidate, Names);
        false -> Candidate
    end.

partition_protos([{file, _Proto}=Item | Rest], Name, Curr, All) ->
    partition_protos(Rest, Name, [Item], add_to_all(Curr, All));
partition_protos([Item | Rest], Name, Curr, All) ->
    partition_protos(Rest, Name, [Item | Curr], All);
partition_protos([], Name, Curr, All) ->
    All2 = lists:reverse(add_to_all(Curr, All)),
    [case Defs of
         [{file, Names} | _] ->
             {Names, Defs};
         _Other ->
             %% file missing, eg compiled from gpb_compile:proto_defs/2,3
             {Name, Defs}
     end
     || Defs <- All2].

add_to_all([], All)   -> All;
add_to_all(Curr, All) -> [lists:reverse(Curr) | All].

partitioned_defs_to_file_descr_set(Defses, Opts) ->
    Infos = [{SansExt, defs_to_file_descr_proto(Name, Defs, Opts)}
             || {{SansExt, Name}, Defs} <- Defses],
    FileDescrProtos = [FdProto || {_SansExt, FdProto} <- Infos],
    Set = #'FileDescriptorSet'{file = FileDescrProtos},
    {Set, Infos}.

defs_to_file_descr_proto(Name, Defs, Opts) ->
    Name1 = compute_filename_from_package(Name, Defs),
    {Pkg, Root, Adjuster} = compute_adjustment_from_package(Defs, Opts),
    Defs1 = process_refs(Defs, Adjuster),
    {TypesToPseudoMsgNames, PseudoMsgs} =
        compute_map_field_pseudo_msgs(Defs1, Opts, Adjuster),
    MsgOptions = collect_msg_options(Defs),
    EnumOptions = collect_enum_options(Defs),
    ServiceOptions = collect_service_options(Defs),
    {ExtOrigins, ExtMsgs} = collect_ext_origins(Defs),
    {ReservedNums, ReservedNames} = collect_reserved_nums_names(Defs),
    ExtRanges = collect_extension_ranges(Defs),
    ToDefsEnv = #to_defs_env{pseudo_msg_names = TypesToPseudoMsgNames,
                             msg_options = MsgOptions,
                             enum_options = EnumOptions,
                             ext_origins = ExtOrigins,
                             ext_msgs = ExtMsgs,
                             ext_ranges = ExtRanges,
                             reserved_nums = ReservedNums,
                             reserved_names = ReservedNames,
                             name_adjuster = Adjuster},
    {Msgs, Enums} = nest_defs_to_types(Defs1, ToDefsEnv),
    RootPath = gpb_lib:string_lexemes(Root, "."),
    RootExts = extensions_to_types(RootPath, ToDefsEnv),
    MapMsgs = maptype_defs_to_msgtype(PseudoMsgs, ToDefsEnv),
    #'FileDescriptorProto'{
       name             = Name1,     %% string() | undefined
       package          = Pkg,
       dependency       = [Dep || {import, Dep} <- Defs], %% [string()]
       message_type     = Msgs ++ MapMsgs,
       enum_type        = Enums,
       service          = defs_to_service(Defs1, ServiceOptions),
       extension        = RootExts,
       options          = file_options(Defs), %% #'FileOptions'{} | undefined
       source_code_info = undefined, %% #'SourceCodeInfo'{} | undefined
       syntax           = proplists:get_value(syntax, Defs1, "proto2")
      }.

compute_filename_from_package(Name, Defs) ->
    case lists:keyfind(package, 1, Defs) of
        {package, Pkg} ->
            PkgPath = dot_to_slash(atom_to_ustring(Pkg)),
            if Name /= undefined -> PkgPath ++ "/" ++ Name;
               Name == undefined -> PkgPath ++ "/" ++ "undefined.proto"
            end;
        false ->
            if Name /= undefined -> Name;
               Name == undefined -> "undefined.proto"
            end
    end.

dot_to_slash("."++Rest)  -> "/"++dot_to_slash(Rest);
dot_to_slash([C | Rest]) -> [C | dot_to_slash(Rest)];
dot_to_slash("")         -> "".

compute_adjustment_from_package(Defs, Opts) ->
    case lists:keyfind(package, 1, Defs) of
        {package, Pkg} ->
            DescriptorPkg = atom_to_ustring(Pkg),
            case proplists:get_bool(use_packages, Opts) of
                true ->
                    {DescriptorPkg, DescriptorPkg, fun root_ref/1};
                false ->
                    PkgCs = name_to_components(Pkg),
                    RefOp = fun(Ref) -> root_ref(prepend_pkg(Ref, PkgCs)) end,
                    {DescriptorPkg, ".", RefOp}
            end;
        false ->
            {undefined, ".", fun root_ref/1}
    end.

process_refs(Defs, RefOp) ->
    lists:map(
      fun({{msg_containment,Proto},MsgNames}) ->
              MsgNames1 = [do_name_ref(Name, RefOp) || Name <- MsgNames],
              {{msg_containment,Proto}, MsgNames1};
         ({{service_containment,Proto}, ServiceNames}) ->
              ServiceNames1 = [do_name_ref(Name, RefOp)
                               || Name <- ServiceNames],
              {{service_containment,Proto}, ServiceNames1};
         ({{rpc_containment,Proto}, Infos}) ->
              Infos1 = [{do_name_ref(ServiceName, RefOp), RpcName}
                        || {ServiceName, RpcName} <- Infos],
              {{rpc_containment,Proto}, Infos1};
         ({{enum_containment,Proto}, EnumNames}) ->
              EnumNames1 = [do_name_ref(Name, RefOp) || Name <- EnumNames],
              {{enum_containment,Proto}, EnumNames1};
         ({{msg,MsgName},Fields}) ->
              {{msg,MsgName}, do_field_refs(Fields, RefOp)};
         ({{group,GName},Fields}) ->
              {{group,GName}, do_field_refs(Fields, RefOp)};
         ({{enum,EnumName}, Enums}) ->
              {{enum,EnumName}, Enums};
         ({{extensions, MsgName}, FieldNumberExtensions}) ->
              {{extensions, MsgName}, FieldNumberExtensions};
         ({{extend, MsgName}, MoreFields}) ->
              MsgName1 = do_name_ref(MsgName, RefOp),
              {{extend, MsgName1}, do_field_refs(MoreFields, RefOp)};
         ({proto3_msgs, MsgNames}) ->
              MsgNames1 = [do_name_ref(Name, RefOp) || Name <- MsgNames],
              {proto3_msgs, MsgNames1};
         ({{reserved_numbers, MsgName}, Reservations}) ->
              MsgName1 = do_name_ref(MsgName, RefOp),
              {{reserved_numbers, MsgName1}, Reservations};
         ({{reserved_names, MsgName}, FieldNames}) ->
              MsgName1 = do_name_ref(MsgName, RefOp),
              {{reserved_names, MsgName1}, FieldNames};
         ({{service,ServiceName}, Rpcs}) ->
              Rpcs1 = do_rpc_refs(Rpcs, RefOp),
              {{service,ServiceName}, Rpcs1};
         (Other) ->
              Other
      end,
      Defs).

do_field_refs(Fields, RefOp) ->
    lists:map(
      fun(#?gpb_field{type={msg,MsgName}}=F) ->
              MsgName1 = do_name_ref(MsgName, RefOp),
              F#?gpb_field{type = {msg, MsgName1}};
         (#?gpb_field{type={group,GName}}=F) ->
              GName1 = do_name_ref(GName, RefOp),
              F#?gpb_field{type = {group, GName1}};
         (#?gpb_field{type={enum,EnumName}}=F) ->
              EnumName1 = do_name_ref(EnumName, RefOp),
              F#?gpb_field{type = {enum, EnumName1}};
         (#?gpb_field{type={map,K,{msg, MsgName}}}=F) ->
              MsgName1 = do_name_ref(MsgName, RefOp),
              F#?gpb_field{type = {map, K, {msg, MsgName1}}};
         (#?gpb_field{type={map,K,{enum, EnumName}}}=F) ->
              EnumName1 = do_name_ref(EnumName, RefOp),
              F#?gpb_field{type = {map, K, {enum, EnumName1}}};
         (#gpb_oneof{fields=Fs}=F) ->
              Fs1 = do_field_refs(Fs, RefOp),
              F#gpb_oneof{fields=Fs1};
         (Other) ->
              Other
      end,
      Fields).

do_rpc_refs(Rpcs, RefOp) ->
    [R#?gpb_rpc{input = do_name_ref(Input, RefOp),
                output = do_name_ref(Output, RefOp)}
     || #?gpb_rpc{input=Input, output=Output}=R <- Rpcs].

name_to_components(Name) ->
    gpb_lib:string_lexemes(atom_to_list(Name), ".").

do_name_ref(Name, RefOp) when is_atom(Name) ->
    RefOp(Name).

prepend_pkg(Name, PkgComponents) ->
    list_to_atom(gpb_lib:dot_join(PkgComponents ++ name_to_components(Name))).

root_ref(Name) ->
    list_to_atom("." ++ atom_to_list(Name)).

nest_defs_to_types(Defs, ToDefsEnv) ->
    NCs = lists:sort(
            lists:foldl(
              fun({{msg,Name}, _}=Def, Acc) ->
                      [{name_to_components(Name), Def} | Acc];
                 ({{group,Name}, _}=Def, Acc) ->
                      [{name_to_components(Name), Def} | Acc];
                 ({{enum,Name}, _}=Def, Acc) ->
                      [{name_to_components(Name), Def} | Acc];
                 (_Other, Acc) -> Acc
              end,
              [],
              Defs)),
    Nestings = mk_prefix_tree(NCs, []),
    defs_to_types(Nestings, ToDefsEnv, []).

defs_to_types([{{Path, {{_msg_or_group, MsgName}, Fields0}}, ChildItems}
               | Rest],
              #to_defs_env{ext_msgs=ExtMsgs,
                           msg_options=MsgOptionsByName,
                           ext_ranges=ExtRangesByMsg,
                           reserved_nums=ReservedNumsByMsg,
                          reserved_names=ReservedNamesByMsg}=ToDefsEnv,
              Acc) when _msg_or_group == msg;
                        _msg_or_group == group ->
    Extendsings = extensions_to_types(Path, ToDefsEnv),
    ExtFields = dict_fetch_list(Path, ExtMsgs),
    ExtRanges = extension_ranges(Path, ExtRangesByMsg),
    Fields1 = remove_extended_fields(Fields0, ExtFields),
    OneofNames = oneof_names_synthetic_last(Fields1),
    ReservedRanges = reserved_ranges(Path, ReservedNumsByMsg),
    ReservedNames = dict_fetch_list(Path, ReservedNamesByMsg),
    {SubMsgs, SubEnums} = defs_to_types(ChildItems, ToDefsEnv, []),
    Item = #'DescriptorProto'{
              name            = atom_to_ustring_base(MsgName),
              field           = field_defs_to_msgtype_fields(
                                  Fields1, OneofNames, ToDefsEnv),
              extension       = Extendsings,
              nested_type     = SubMsgs,
              enum_type       = SubEnums,
              extension_range = ExtRanges,
              reserved_range  = ReservedRanges,
              reserved_name   = ReservedNames,
              options         = msg_options(MsgName, MsgOptionsByName),
              oneof_decl      = oneof_decl(OneofNames)},
    defs_to_types(Rest, ToDefsEnv, [Item | Acc]);
defs_to_types([{{_Path, {{enum, EnumName}, Enumerators}}, []} | Rest],
              #to_defs_env{enum_options=EnumOptionsByName}=ToDefsEnv,
              Acc) ->
    Item = #'EnumDescriptorProto'{
              name  = atom_to_ustring_base(EnumName),
              value = [#'EnumValueDescriptorProto'{
                          name   = atom_to_ustring(EName),
                          number = EValue,
                          options = enum_value_options(EOpts)}
                       || {EName, EValue, EOpts} <- Enumerators],
              options = enum_options(EnumName, EnumOptionsByName)},
    defs_to_types(Rest, ToDefsEnv, [Item | Acc]);
defs_to_types([], _ToDefsEnv, Acc) ->
    lists:partition(fun(#'DescriptorProto'{}) -> true;
                       (#'EnumDescriptorProto'{}) -> false
                    end,
                    lists:reverse(Acc)).

remove_extended_fields(Fields, []) ->
    %% Short-circuit the common case:
    Fields;
remove_extended_fields(Fields, ExtFields) ->
    ExtFNums = [FNum || #?gpb_field{fnum=FNum} <- ExtFields],
    lists:filter(
      fun(#?gpb_field{fnum=FNum}) ->
              not lists:member(FNum, ExtFNums);
         (#gpb_oneof{}) ->
              %% A oneof cannot contain 'extend'ed fields; keep it.
              true
      end,
      Fields).

oneof_names_synthetic_last(Fields) ->
    %% Synthetic oneof fields must come after all non-syntetic ones
    Oneofs = [Field || #gpb_oneof{}=Field <- Fields],
    {Synthetics, Nonsynthetics} = lists:partition(fun is_synthetic_oneof/1,
                                                  Oneofs),
    NonsyntheticNames = [Name || #gpb_oneof{name=Name} <- Nonsynthetics],
    SyntheticNames = [Name || #gpb_oneof{name=Name} <- Synthetics],
    NonsyntheticNames ++ SyntheticNames.

is_synthetic_oneof(Field) ->
    %% If it contains only one item with occurrence=defaulty
    case Field of
        #gpb_oneof{fields=[#?gpb_field{occurrence=defaulty}]} ->
            true;
        _ ->
            false
    end.

extension_ranges(Path, ExtRangesByMsg) ->
    ExtRanges = dict_fetch_list(Path, ExtRangesByMsg),
    [#'DescriptorProto.ExtensionRange'{start = Start, % inclusive
                                       'end' = range_end(End)} % exclusive
     || {Start, End} <- ExtRanges].

reserved_ranges(Path, NumsByMsg) ->
    [begin
         {Start, End} = if is_integer(Num) -> {Num, Num};
                           is_tuple(Num) -> Num
                        end,
         #'DescriptorProto.ReservedRange'{start = Start, % inclusive
                                          'end' = range_end(End)} % exclusive
     end
     || Num <- dict_fetch_list(Path, NumsByMsg)].

range_end(max) -> 16#20000000; % 536870912
range_end(Max) when is_integer(Max) -> Max + 1. % inclusive -> exclusive

maptype_defs_to_msgtype(MapPseudoMsgs, ToDefsEnv) ->
    [#'DescriptorProto'{
        name            = atom_to_ustring_base(MsgName),
        field           = field_defs_to_msgtype_fields(
                            Fields, [], ToDefsEnv),
        extension       = [],
        nested_type     = [],
        enum_type       = [],
        extension_range = [],
        options         = #'MessageOptions'{map_entry=true},
        oneof_decl      = []
       }
     || {{msg,MsgName}, Fields} <- MapPseudoMsgs].

extensions_to_types(Path, #to_defs_env{ext_origins=ExtOrigins,
                                       name_adjuster=NameAdjuster}=ToDefsEnv) ->
    Exts = dict_fetch_list(Path, ExtOrigins),
    %% FIXME: is it possible to extend with oneofs alternatives?
    %% Is it possible to extend with new oneofs fields?
    AllOneofs = [],

    lists:append(
      [set_extendee(NameAdjuster(Extendee),
                    field_defs_to_msgtype_fields(Fields, AllOneofs, ToDefsEnv))
       || {Extendee, Fields} <- Exts]).

set_extendee(Extendee, FieldDescrs) when is_list(FieldDescrs) ->
    [FieldDescr#'FieldDescriptorProto'{extendee = atom_to_list(Extendee)}
     || FieldDescr <- FieldDescrs].

field_defs_to_msgtype_fields(Fields, AllOneofs, ToDefsEnv) ->
    lists:append([field_def_to_msgtype_field(Field, AllOneofs,
                                             ToDefsEnv, undefined)
                  || Field <- Fields]).

field_def_to_msgtype_field(#?gpb_field{name=FName,
                                       fnum=FNum,
                                       type=Type,
                                       occurrence=Occurrence,
                                       opts=Opts}=Field,
                           _AllOneofs,
                           #to_defs_env{
                              pseudo_msg_names=MapTypesToPseudoMsgNames},
                           Proto3Optional) ->
    [#'FieldDescriptorProto'{
        name          = atom_to_ustring_base(FName),
        number        = FNum,
        label         = occurrence_def_to_descr_label(Occurrence),
        type          = type_to_descr_type(Type),
        type_name     = type_to_descr_type_name(Type, MapTypesToPseudoMsgNames),
        default_value = field_default_value(Field),
        json_name     = proplists:get_value(json_name, Opts),
        options       = field_options(Opts),
        proto3_optional = Proto3Optional}];
field_def_to_msgtype_field(#gpb_oneof{name=FName,
                                      fields=OFields}=Field,
                           AllOneofs,
                           MapTypesToPseudoMsgNames,
                           _Proto3Optional) ->
    Proto3Optional = case is_synthetic_oneof(Field) of
                         true  -> true;
                         false -> undefined
                     end,
    OneofIndex = find_oneof_index(FName, AllOneofs),
    [begin
         [F] = field_def_to_msgtype_field(OField, AllOneofs,
                                          MapTypesToPseudoMsgNames,
                                          Proto3Optional),
         F#'FieldDescriptorProto'{oneof_index = OneofIndex}
     end
     || OField <- OFields].

find_oneof_index(Name, Names) ->
    find_pos(Name, Names, 0).

find_pos(Name, [Name | _], Pos) -> Pos;
find_pos(Name, [_ | Rest], Pos) -> find_pos(Name, Rest, Pos+1).

occurrence_def_to_descr_label(optional) -> 'LABEL_OPTIONAL';
occurrence_def_to_descr_label(defaulty) -> 'LABEL_OPTIONAL';
occurrence_def_to_descr_label(required) -> 'LABEL_REQUIRED';
occurrence_def_to_descr_label(repeated) -> 'LABEL_REPEATED'.

type_to_descr_type(sint32)           -> 'TYPE_SINT32';
type_to_descr_type(sint64)           -> 'TYPE_SINT64';
type_to_descr_type(int32)            -> 'TYPE_INT32';
type_to_descr_type(int64)            -> 'TYPE_INT64';
type_to_descr_type(uint32)           -> 'TYPE_UINT32';
type_to_descr_type(uint64)           -> 'TYPE_UINT64';
type_to_descr_type(bool)             -> 'TYPE_BOOL';
type_to_descr_type({enum,_EnumName}) -> 'TYPE_ENUM';
type_to_descr_type(fixed64)          -> 'TYPE_FIXED64';
type_to_descr_type(sfixed64)         -> 'TYPE_SFIXED64';
type_to_descr_type(double)           -> 'TYPE_DOUBLE';
type_to_descr_type(string)           -> 'TYPE_STRING';
type_to_descr_type(bytes)            -> 'TYPE_BYTES';
type_to_descr_type({msg,_MsgName})   -> 'TYPE_MESSAGE';
type_to_descr_type({group,_Name})    -> 'TYPE_GROUP';
type_to_descr_type(fixed32)          -> 'TYPE_FIXED32';
type_to_descr_type(sfixed32)         -> 'TYPE_SFIXED32';
type_to_descr_type(float)            -> 'TYPE_FLOAT';
type_to_descr_type({map,_,_})        -> 'TYPE_MESSAGE'.

type_to_descr_type_name({msg,MsgName}, _)   -> atom_to_ustring(MsgName);
type_to_descr_type_name({group,Name}, _)    -> atom_to_ustring(Name);
type_to_descr_type_name({enum,EnumName}, _) -> atom_to_ustring(EnumName);
type_to_descr_type_name({map,_,_}=T, M)     -> atom_to_ustring(dict:fetch(T,M));
type_to_descr_type_name(_, _)               -> undefined.

field_default_value(#?gpb_field{type=Type, opts=Opts}) ->
    case {Type, proplists:get_value(default, Opts)} of
        {_, undefined}     -> undefined;
        {sint32, I}        -> integer_to_list(I);
        {sint64, I}        -> integer_to_list(I);
        {int32, I}         -> integer_to_list(I);
        {int64, I}         -> integer_to_list(I);
        {uint32, I}        -> integer_to_list(I);
        {uint64, I}        -> integer_to_list(I);
        {bool, B}          -> atom_to_list(B);
        {{enum,_EnumName}, E} -> atom_to_ustring(E);
        {fixed64, I}       -> integer_to_list(I);
        {sfixed64, I}      -> integer_to_list(I);
        {double, F}        -> format_float(F);
        {string, S}        -> S;
        {bytes, B}         -> escape_bytes(B);
        {fixed32, I}       -> integer_to_list(I);
        {sfixed32, I}      -> integer_to_list(I);
        {float, F}         -> format_float(F)
    end.

field_options(Opts) ->
    FNames = record_info(fields, 'FieldOptions'),
    set_options(Opts, FNames, #'FieldOptions'{}).

msg_options(MsgName, D) ->
    case dict:find(MsgName, D) of
        error ->
            undefined;
        {ok, []} ->
            undefined;
        {ok, Opts} ->
            FNames = record_info(fields, 'MessageOptions'),
            set_options(Opts, FNames, #'MessageOptions'{})
    end.

enum_options(EnumName, D) ->
    case dict:find(EnumName, D) of
        error ->
            undefined;
        {ok, []} ->
            undefined;
        {ok, Opts} ->
            FNames = record_info(fields, 'EnumOptions'),
            set_options(Opts, FNames, #'EnumOptions'{})
    end.

defs_to_service(Defs, ServiceOptionsByName) ->
    [#'ServiceDescriptorProto'{
        name   = atom_to_ustring_base(ServiceName),
        method = [#'MethodDescriptorProto'{
                     name        = atom_to_ustring(RpcName),
                     input_type  = atom_to_ustring(Input),
                     output_type = atom_to_ustring(Output),
                     client_streaming = InputStream,
                     server_streaming = OutputStream,
                     options     = method_options(RpcOpts)}
                  || #?gpb_rpc{name=RpcName,
                               input=Input,   input_stream=InputStream,
                               output=Output, output_stream=OutputStream,
                               opts=RpcOpts} <- Rpcs],
        options = service_options(ServiceName, ServiceOptionsByName)}
     || {{service, ServiceName}, Rpcs} <- Defs].

oneof_decl(AllOneofs) ->
    [#'OneofDescriptorProto'{name=atom_to_ustring(Name)} || Name <- AllOneofs].

compute_map_field_pseudo_msgs(Defs, Opts, Adjuster) ->
    AllMapTypes = find_all_map_types(Defs),
    PkgBase = case lists:keyfind(package, 1, Defs) of
                  {package, Pkg} ->
                      case proplists:get_bool(use_packages, Opts) of
                          true  -> atom_to_list(Pkg) ++ ".";
                          false -> ""
                      end;
                  false ->
                      ""
              end,
    MapTypePseudoMsgNames = invent_unused_msg_names(Defs, PkgBase,
                                                    length(AllMapTypes)),
    MapTypePseudoMsgNames1 = [Adjuster(Name) || Name <- MapTypePseudoMsgNames],
    ToName = lists:zip(AllMapTypes, MapTypePseudoMsgNames1),
    ToMapT = lists:zip(MapTypePseudoMsgNames1, AllMapTypes),
    {dict:from_list(ToName),
     [{{msg, Name}, gpb:map_item_pseudo_fields(KeyType, ValueType)}
      || {Name, {map, KeyType, ValueType}} <- ToMapT]}.

find_all_map_types(Defs) ->
    lists:sort(
      sets:to_list(
        lists:foldl(
          fun({{msg,_}, Fields}, Acc) ->
                  lists:foldl(fun(#?gpb_field{type={map,_,_}=T}, Acc2) ->
                                      sets:add_element(T, Acc2);
                                 (_, Acc2) ->
                                      Acc2
                              end,
                              Acc,
                              Fields);
             (_, Acc) ->
                  Acc
          end,
          sets:new(),
          Defs))).

invent_unused_msg_names(Defs, PkgBase, N) ->
    AllMsgNames = sets:from_list([Name || {{msg,Name}, _Fields} <- Defs]),
    Base = PkgBase ++ "MapFieldEntry",
    invent_unused_msg_names_aux(Base, 1, N, AllMsgNames).

invent_unused_msg_names_aux(Base, I, N, AllMsgNames) ->
    NewNames = sets:from_list([list_to_atom(?ff("~s_~w_~w", [Base, I, J]))
                               || J <- lists:seq(1,N)]),
    case sets:is_disjoint(NewNames, AllMsgNames) of
        true  -> sets:to_list(NewNames);
        false -> invent_unused_msg_names_aux(Base, I+1, N, AllMsgNames)
    end.

collect_msg_options(Defs) ->
    lists:foldl(
      fun({{msg_options, MsgName}, Opts}, D) ->
              dict:append_list(MsgName, Opts, D);
         (_Other, D) ->
              D
      end,
      dict:new(),
      Defs).

collect_enum_options(Defs) ->
    lists:foldl(
      fun({{enum_options, EnumName}, Opts}, D) ->
              dict:append_list(EnumName, Opts, D);
         (_Other, D) ->
              D
      end,
      dict:new(),
      Defs).

enum_value_options(Opts) ->
    FNames = record_info(fields, 'EnumValueOptions'),
    set_options(Opts, FNames, #'EnumValueOptions'{}).

collect_service_options(Defs) ->
    lists:foldl(
      fun({{service_options, ServiceName}, Opts}, D) ->
              dict:append_list(ServiceName, Opts, D);
         (_Other, D) ->
              D
      end,
      dict:new(),
      Defs).

service_options(ServiceName, D) ->
    case dict:find(ServiceName, D) of
        error ->
            undefined;
        {ok, []} ->
            undefined;
        {ok, Opts} ->
            FNames = record_info(fields, 'ServiceOptions'),
            set_options(Opts, FNames, #'ServiceOptions'{})
    end.

method_options(Opts) ->
    FNames = record_info(fields, 'MethodOptions'),
    set_options(Opts, FNames, #'MethodOptions'{}).

file_options(Defs) ->
    Opts = [{K, V} || {option, K, V} <- Defs],
    FNames = record_info(fields, 'FileOptions'),
    set_options(Opts, FNames, #'FileOptions'{}).

set_options(Opts, FNames, Initial) ->
    Defaults = tl(tuple_to_list(Initial)),
    Positions = lists:seq(2, length(FNames) + 1),
    FDPs = lists:zip3(FNames, Defaults, Positions),
    set_opts2(Opts, FDPs, Initial, false).

set_opts2([Opt | Rest], FDPs, OptMsg, HasChanged) ->
    %% Normalize opts first for safety
    {K, V} = if is_tuple(Opt), tuple_size(Opt) == 2 -> Opt;
                is_atom(Opt) -> {Opt, true}
             end,
    case lists:keyfind(K, 1, FDPs) of
        {K, Default, Pos} ->
            if V == Default ->
                    set_opts2(Rest, FDPs, OptMsg, HasChanged);
               V /= Default ->
                    OptMsg1 = setelement(Pos, OptMsg, V),
                    set_opts2(Rest, FDPs, OptMsg1, true)
            end;
        false ->
            set_opts2(Rest, FDPs, OptMsg, HasChanged)
    end;
set_opts2([], _FNamesDefaults, OptMsg, HasChanged) ->
    if HasChanged     -> OptMsg;
       not HasChanged -> undefined
    end.

collect_ext_origins(Defs) ->
    lists:foldl(
      fun({{ext_origin, Extendee}, {Location, Fields}}, {ByLoc, ByMsg}) ->
              LPath = name_to_components(Location),
              MPath = name_to_components(Extendee),
              ByLoc1 = dict:append(LPath, {Extendee, Fields}, ByLoc),
              ByMsg1 = dict:append_list(MPath, Fields, ByMsg),
              {ByLoc1, ByMsg1};
         (_Other, {ByLoc, ByMsg}) ->
              {ByLoc, ByMsg}
      end,
      {dict:new(), dict:new()},
      Defs).

collect_extension_ranges(Defs) ->
    lists:foldl(
      fun({{extensions, Msg}, Ranges}, D) ->
              dict:store(name_to_components(Msg), Ranges, D);
         (_Other, Acc) ->
              Acc
      end,
      dict:new(),
      Defs).

collect_reserved_nums_names(Defs) ->
    lists:foldl(
      fun({{reserved_numbers, Msg}, Reserved}, {Nums, Names}) ->
              Path = name_to_components(Msg),
              Nums1 = dict:append_list(Path, Reserved, Nums),
              {Nums1, Names};
         ({{reserved_names, Msg}, Reserved}, {Nums, Names}) ->
              Path = name_to_components(Msg),
              Names1 = dict:append_list(Path, Reserved, Names),
              {Nums, Names1};
         (_Other, {Nums, Names}) ->
              {Nums, Names}
      end,
      {dict:new(), dict:new()},
      Defs).

atom_to_ustring(A) ->
    Utf8Str = atom_to_list(A),
    unicode:characters_to_list(list_to_binary(Utf8Str), utf8).

atom_to_ustring_base(A) ->
    Utf8Str = lists:last(gpb_lib:string_lexemes(atom_to_list(A), ".")),
    unicode:characters_to_list(list_to_binary(Utf8Str), utf8).

format_float(F) when is_float(F) -> float_to_list(F);
format_float(infinity)           -> "inf";
format_float('-infinity')        -> "-inf";
format_float('nan')              -> "nan".

escape_bytes(<<B, Rest/binary>>) ->
    if B >= 127 -> escape_char(B) ++ escape_bytes(Rest);
       B < $\s  -> escape_char(B) ++ escape_bytes(Rest);
       true     -> [B | escape_bytes(Rest)]
    end;
escape_bytes(<<>>) ->
    "".

escape_char(C) -> ?ff("\\~.8b", [C]).

dict_fetch_list(Key, D) ->
    case dict:find(Key, D) of
        {ok, Elems} when is_list(Elems) -> Elems;
        error -> []
    end.

mk_prefix_tree([{Path,_Def}=PathDef | Rest], Acc) -> % iterates over sorted list
    {Children, Rest2} =
        lists:splitwith(fun({Path2,_Def2}) -> lists:prefix(Path, Path2) end,
                        Rest),
    ChildTrees = mk_prefix_tree(Children, []),
    mk_prefix_tree(Rest2, [{PathDef, ChildTrees} | Acc]);
mk_prefix_tree([], Acc) ->
    lists:reverse(Acc).

prefix_tree_test() ->
    [{{[a,b],1},
      [{{[a,b,c],0},
        [{{[a,b,c,d],2},[]},
         {{[a,b,c,e],3},[]}]},
       {{[a,b,f],5},
        [{{[a,b,f,g],4},[]}]}]},
     {{[x],-1},[]}] = mk_prefix_tree(lists:sort(
                                       [{[a,b], 1},
                                        {[a,b,c], 0},
                                        {[a,b,c,d], 2},
                                        {[a,b,c,e], 3},
                                        {[a,b,f], 5},
                                        {[a,b,f,g], 4},
                                        {[x], -1}]),
                                     []).
