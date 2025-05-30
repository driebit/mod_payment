%% @copyright 2018-2024 Driebit BV
%% @doc Main payment model and SQL definitions. Maintains a single table of all
%% payments. All PSP modules store their payments in this table, including extra
%% PSP specific properties.
%% @end

%% Copyright 2018-2024 Driebit BV
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_payment).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    list_user/2,

    insert/2,
    insert_recurring_payment/4,
    insert_recurring_payment/5,
    get/2,
    get_by_psp/3,
    update_psp_handler/3,
    payment_psp_view_url/2,
    set_payment_status/3,
    set_payment_status/4,
    cancel_recurring_payment/2,

    search_query/2,
    list_status_check/1,

    install/1,
    delete_all_payments/1,
    indices/2
]).

-include_lib("zotonic.hrl").
-include("../include/payment.hrl").

m_find_value(redirect_psp, #m{ value = undefined } = M, _Context) ->
    M#m{ value = redirect_psp };
m_find_value(PaymentNr, #m{ value = redirect_psp }, Context) ->
    case payment_psp_view_url(PaymentNr, Context) of
        {ok, Url} -> Url;
        {error, _} -> undefined
    end;
m_find_value(list_user, #m{ value = undefined } = M, _Context) ->
    M#m{ value = list_user };
m_find_value(UserId, #m{ value = list_user }, Context) ->
    list_user(UserId, Context);
m_find_value(PaymentNr, #m{ value = undefined }, Context) ->
    case get(z_convert:to_binary(PaymentNr), Context) of
        {ok, Props} -> Props;
        {error, _} -> undefined
    end.

m_to_list(#m{ value = list_user }, Context) ->
    list_user(z_acl:user(Context), Context);
m_to_list(#m{}, _Context) ->
    [].

m_value(#m{ value = list_user }, Context) ->
    list_user(z_acl:user(Context), Context);
m_value(#m{}, _Context) ->
    undefined.

%% @doc Fetch all payments of an user, newest first
list_user(UserId, Context) ->
    L = z_db:assoc("
        select *
        from payment
        where user_id = $1
        order by created desc",
        [UserId],
        Context),
    lists:map(
        fun(P) ->
            add_status_flags(P)
        end,
        L).

%% @doc Create new payment.
insert(PaymentReq, Context) ->
    UserId = case PaymentReq#payment_request.user_id of
        undefined -> z_acl:user(Context);
        UId when is_integer(UId) -> UId
    end,
    PaymentNr = z_convert:to_binary(z_ids:identifier(32)),
    DescrHTML = case PaymentReq#payment_request.description_html of
        undefined -> <<>>;
        RDHtml -> z_sanitize:html(RDHtml, Context)
    end,
    Descr = case PaymentReq#payment_request.description of
        undefined -> z_string:trim(z_html:strip(DescrHTML));
        RD -> RD
    end,
    Props = [
        {user_id, UserId},
        {payment_nr, PaymentNr},
        {is_recurring_start, PaymentReq#payment_request.is_recurring_start},
        {key, PaymentReq#payment_request.key},
        {currency, PaymentReq#payment_request.currency},
        {amount, PaymentReq#payment_request.amount},
        {description, z_string:truncate(Descr, 60)},
        {description_html, DescrHTML},
        {language, language(UserId, Context)}
    ] ++ naw_props(UserId, PaymentReq#payment_request.is_qargs, Context)
      ++ extra_props(PaymentReq#payment_request.extra_props, PaymentReq#payment_request.is_qargs, Context),
    case validate_payment(Props) of
        ok ->
            z_db:insert(payment, Props, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Insert a recurring payment, automatically paid via the PSP. The RecurringPaymentId
%% refers to the recurring payment that started the subscription.
insert_recurring_payment(RecurringPaymentId, Currency, Amount, Context) ->
    insert_recurring_payment(RecurringPaymentId, erlang:universaltime(), Currency, Amount, Context).

insert_recurring_payment(RecurringPaymentId, Created, Currency, Amount, Context) ->
    case ?MODULE:get(RecurringPaymentId, Context) of
        {ok, Payment} ->
            Currency1 = case Currency of
                undefined -> proplists:get_value(currency, Payment);
                _ -> Currency
            end,
            Props = [
                {payment_nr, z_convert:to_binary(z_ids:identifier(32))},
                {recurring_payment_id, RecurringPaymentId},
                {psp_module, proplists:get_value(psp_module, Payment)},
                {user_id, proplists:get_value(user_id, Payment)},
                {is_recurring_start, false},
                {key, proplists:get_value(key, Payment)},
                {currency, Currency1},
                {amount, z_convert:to_float(Amount)},
                {description, proplists:get_value(description, Payment)},
                {description_html, proplists:get_value(description_html, Payment)},
                {name_first, proplists:get_value(name_first, Payment)},
                {name_surname_prefix, proplists:get_value(name_surname_prefix, Payment)},
                {name_surname, proplists:get_value(name_surname, Payment)},
                {address_street_1, proplists:get_value(address_street_1, Payment)},
                {address_street_2, proplists:get_value(address_street_2, Payment)},
                {address_postcode, proplists:get_value(address_postcode, Payment)},
                {address_city, proplists:get_value(address_city, Payment)},
                {address_state, proplists:get_value(address_state, Payment)},
                {address_country, proplists:get_value(address_country, Payment)},
                {email, proplists:get_value(email, Payment)},
                {phone, proplists:get_value(phone, Payment)},
                {created, Created}
            ],
            case validate_payment(Props) of
                ok ->
                    z_db:insert(payment, Props, Context);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

% Extra properties
extra_props(Props, false, _Context) ->
    Props1 = lists:filter(
        fun
            ({_, <<>>}) -> false;
            ({_, undefined}) -> false;
            ({_, ""}) -> false;
            ({_, _}) -> true;
            (_) -> false
        end,
        Props),
    z_sanitize:escape_props_check(Props1);
extra_props(Props, true, Context) ->
    Props1 = lists:map(
        fun
            ({K, _} = KV) ->
                case z_context:get_q(K, Context) of
                    undefined -> KV;
                    V -> {K, V}
                end;
            (K) when is_atom(K) ->
                {K, z_context:get_q(K, Context)}
        end,
        Props),
    extra_props(Props1, false, Context).

% We should have an email address, or an user-id, and name + phone no.
validate_payment(Props) ->
    Validations = [
        {amount, fun() ->
                    is_number(proplists:get_value(amount, Props))
                    andalso proplists:get_value(amount, Props) > 0
                 end},
        {email,  fun() -> is_email_address( proplists:get_value(email, Props) ) end},
        {contact, fun() ->
                    is_integer(proplists:get_value(user_id, Props))
                    orelse (not z_utils:is_empty(proplists:get_value(name_surname, Props)))
                  end}
    ],
    check(Validations).

check([]) ->
    ok;
check([ {Reason, F} | Vs ]) ->
    case F() of
        true -> check(Vs);
        false -> {error, Reason}
    end.

is_email_address(undefined) ->
    false;
is_email_address(Email) ->
    z_email_utils:is_email(Email).

naw_props(UserId, IsQArgs, Context) ->
    Naw = [
        {P, p(UserId, P, IsQArgs, Context)}
        || P <- [
            name_first,
            name_surname_prefix,
            name_surname,
            address_street_1,
            address_street_2,
            address_city,
            address_state,
            address_country,
            address_postcode,
            email,
            phone
        ]
    ],
    lists:filter( fun({_,V}) -> V =/= undefined end, Naw ).

p(Id, Prop, true, Context) ->
    case z_context:get_q(Prop, Context) of
        undefined ->
            p(Id, Prop, false, Context);
        V ->
            V1 = z_html:escape_check( z_string:trim(V) ),
            z_string:truncate(V1, 200, <<>>)
    end;
p(Id, Prop, false, Context) ->
    case m_rsc:p_no_acl(Id, Prop, Context) of
        undefined -> undefined;
        V -> z_string:truncate(z_string:trim(V), 200, <<>>)
    end.

language(undefined, Context) ->
    z_context:language(Context);
language(UserId, Context) ->
    case m_rsc:p(UserId, pref_language, Context) of
        undefined -> z_context:language(Context);
        Lang -> Lang
    end.

-spec get(integer()|binary(), z:context()) -> {ok, proplists:proplist()} | {error, term()}.
get(PaymentId, Context) when is_integer(PaymentId) ->
    case z_db:assoc_row(
        "select * from payment where id = $1",
        [PaymentId],
        Context)
    of
        undefined -> {error, notfound};
        Props -> {ok, add_status_flags(Props)}
    end;
get(PaymentNr, Context) when is_binary(PaymentNr); is_list(PaymentNr) ->
    case z_db:assoc_row(
        "select * from payment where payment_nr = $1",
        [PaymentNr],
        Context)
    of
        undefined -> {error, notfound};
        Props -> {ok, add_status_flags(Props)}
    end.

-spec get_by_psp(atom(), binary()|string(), z:context()) -> {ok, proplists:proplist()} | {error, term()}.
get_by_psp(_Module, <<>>, _Context) ->
    {error, notfound};
get_by_psp(_Module, "", _Context) ->
    {error, notfound};
get_by_psp(_Module, undefined, _Context) ->
    {error, notfound};
get_by_psp(PspModule, PspExternalId, Context) ->
    case z_db:assoc_row(
        "select * from payment where psp_module = $1 and psp_external_id = $2",
        [PspModule, PspExternalId],
        Context)
    of
        undefined -> {error, notfound};
        Props -> {ok, add_status_flags(Props)}
    end.

add_status_flags(Props) ->
    Status = bin_to_atom(proplists:get_value(status, Props)),
    [
        {status, Status},
        {is_paid, is_paid_status(Status)},
        {is_failed, is_failed_status(Status)},
        {psp_module, z_convert:to_atom(proplists:get_value(psp_module, Props))}
        | proplists:delete(status,
            proplists:delete(psp_module, Props))
    ].

bin_to_atom(undefined) -> undefined;
bin_to_atom(A) when is_atom(A) -> A;
bin_to_atom(B) when is_binary(B) -> erlang:binary_to_atom(B, utf8).

is_paid_status(paid) -> true;
is_paid_status(_) -> false.

is_failed_status(error) -> true;
is_failed_status(failed) -> true;
is_failed_status(cancelled) -> true;
is_failed_status(refunded) -> true;
is_failed_status(_) -> false.


-spec update_psp_handler(integer(), #payment_psp_handler{}, z:context()) -> ok | {error, term()}.
update_psp_handler(PaymentId, Handler, Context) ->
    case z_db:q("
        update payment
        set psp_module = $2,
            psp_external_id = $3,
            psp_payment_description = $4,
            psp_data = $5
        where id = $1
        ",
        [
            PaymentId,
            Handler#payment_psp_handler.psp_module,
            Handler#payment_psp_handler.psp_external_id,
            Handler#payment_psp_handler.psp_payment_description,
            ?DB_PROPS(Handler#payment_psp_handler.psp_data)
        ],
        Context)
    of
        1 -> ok;
        0 -> {error, notfound}
    end.

-spec set_payment_status(integer(), atom(), z:context()) -> {ok, changed|unchanged} | {error, term()}.
set_payment_status(PaymentId, Status, Context) ->
    Now = calendar:universal_time(),
    set_payment_status(PaymentId, Status, Now, Context).

-spec set_payment_status(integer(), atom(), calendar:datetime(), z:context()) ->
    {ok, changed|unchanged} | {error, term()}.
set_payment_status(PaymentId, Status, StatusDate, Context) ->
    z_db:transaction(
        fun(Ctx) ->
            CurrStatusBin = z_db:q1("
                select status
                from payment
                where id = $1",
                [PaymentId],
                Ctx),
            case z_convert:to_atom(CurrStatusBin) of
                undefined ->
                    {error, notfound};
                Status ->
                    {ok, unchanged};
                _OldStatus ->
                    case z_db:q("
                        update payment
                        set status = $1::varchar,
                            status_date = $3
                        where id = $2
                          and status <> $1::varchar
                          and (   status_date is null
                               or status_date <= $3)",
                        [z_convert:to_binary(Status), PaymentId, StatusDate],
                        Ctx)
                    of
                        0 -> {ok, unchanged};
                        1 -> {ok, changed}
                    end
            end
        end,
        Context).

search_query({Offset, Limit}, Context) ->
    Rows = z_db:assoc("
        select *
        from payment
        order by created desc
        offset $1
        limit $2",
        [Offset-1, Limit],
        Context),
    #search_result{
        result = Rows,
        total = total(Context)
    }.

total(Context) ->
    z_db:q1("select count(*) from payment", Context).


%% @doc Return a list of all payments that are in a temporary status for longer
%%      than an hour.
-spec list_status_check( z:context() ) -> list( proplists:proplist() ).
list_status_check(Context) ->
    LastHour = z_datetime:prev_hour( calendar:universal_time() ),
    z_db:assoc("
            select *
            from payment
            where status in ('new', 'pending')
              and (   (status_date is null and created < $1)
                   or status_date < $1)
            order by id
        ",
        [ LastHour ],
        Context).

-spec payment_psp_view_url(binary()|integer(), z:context()) -> {ok, binary()} | {error, term()}.
payment_psp_view_url(PaymentId, Context) ->
    case get(PaymentId, Context) of
        {ok, Payment} ->
            Req = #payment_psp_view_url{
                payment_id = proplists:get_value(id, Payment),
                psp_module = proplists:get_value(psp_module, Payment),
                psp_external_id = proplists:get_value(psp_external_id, Payment),
                psp_data = proplists:get_value(psp_data, Payment)
            },
            case z_notifier:first(Req, Context) of
                undefined -> {error, unsupported};
                {ok, Uri} -> {ok, Uri}
            end;
        {error, _} = Error ->
            Error
    end.

-spec delete_all_payments(z:context()) -> ok.
delete_all_payments(Context) ->
    z_db:q("delete from payment_log", Context),
    z_db:q("delete from payment", Context),
    ok.

-spec install(z:context()) -> ok.
install(Context) ->
    case z_db:table_exists(payment, Context) of
        false ->
            [] = z_db:q("
                create table payment (
                    id serial not null,
                    user_id int,
                    payment_nr character varying(64) not null,
                    status character varying(16) not null default 'new',
                    status_date timestamp,
                    is_recurring_start boolean not null default false,
                    recurring_payment_id int,

                    psp_module character varying(64),
                    psp_external_id character varying(64),
                    psp_payment_description text,
                    psp_data bytea,

                    key character varying(256),
                    language character varying(16) not null default 'en',
                    description character varying(64) not null default '',
                    description_html text not null default '',

                    name_first character varying(256),
                    name_surname_prefix character varying(256),
                    name_surname character varying(256),
                    address_street_1 character varying(256),
                    address_street_2  character varying(256),
                    address_postcode character varying(256),
                    address_city character varying(256),
                    address_state character varying(256),
                    address_country character varying(8),
                    email character varying(256),
                    phone character varying(256),

                    props bytea,

                    currency character varying(10) not null default 'EUR',
                    amount float not null,

                    created timestamp with time zone NOT NULL DEFAULT now(),
                    modified timestamp with time zone NOT NULL DEFAULT now(),

                    constraint payment_pkey primary key (id),
                    constraint payment_payment_nr unique (payment_nr),
                    constraint payment_user_id
                        foreign key (user_id)
                        references rsc (id)
                        on update cascade on delete set null
                )", Context),
            [] = z_db:q("
                create index payment_created_key
                on payment (created)",
                Context),
            [] = z_db:q("
                create index fki_payment_user_id
                on payment (user_id)",
                Context),
            [] = z_db:q("
                create index payment_status_key
                on payment (status)",
                Context),
            [] = z_db:q("
                create index payment_psp_external_id_key
                on payment (psp_external_id)",
                Context),
            [] = z_db:q("
                create index fki_payment_recurring_payment_id
                on payment (recurring_payment_id)",
                Context),
            [] = z_db:q("
                ALTER TABLE payment
                ADD CONSTRAINT fk_payment_recurring_payment_id FOREIGN KEY (recurring_payment_id)
                REFERENCES payment (id)
                ON UPDATE CASCADE ON DELETE RESTRICT",
                Context),
            ok;
        true ->
            add_recurring_payment_id_column(Context),
            add_recurring_column(Context),
            add_status_date_column(Context),
            case lists:member(<<"payment_created_key">>, indices("payment", Context)) of
                true ->
                    ok;
                false ->
                    [] = z_db:q("
                        create index payment_created_key
                        on payment (created)",
                        Context)
            end,
            ok
    end.

indices(Table, Context) ->
    Is = z_db:q("
        select i.relname
        from pg_class t,
             pg_class i,
             pg_index ix,
             pg_attribute a
        where t.oid = ix.indrelid
          and i.oid = ix.indexrelid
          and a.attrelid = t.oid
          and a.attnum = ANY(ix.indkey)
          and t.relkind = 'r'
          and t.relname = $1
        ", [ Table ], Context),
    [ I || {I} <- Is ].


%% @doc Add recurring_payment_id column if it was not yet present
add_recurring_payment_id_column(Context) ->
    case lists:member(recurring_payment_id, z_db:column_names(payment, Context)) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                ALTER TABLE payment
                ADD COLUMN recurring_payment_id int
                ", Context),
            [] = z_db:q("
                create index fki_payment_recurring_payment_id
                on payment (recurring_payment_id)",
                Context),
            [] = z_db:q("
                ALTER TABLE payment
                ADD CONSTRAINT fk_payment_recurring_payment_id FOREIGN KEY (recurring_payment_id)
                REFERENCES payment (id)
                ON UPDATE CASCADE ON DELETE RESTRICT",
                Context),
            z_db:flush(Context),
            ok
    end.


%% @doc Add recurring column if it was not yet present
add_recurring_column(Context) ->
    case lists:member(is_recurring_start, z_db:column_names(payment, Context)) of
        true ->
            ok;
        false ->
            case lists:member(recurring, z_db:column_names(payment, Context)) of
                true ->
                    [] = z_db:q("
                        ALTER TABLE payment
                        RENAME COLUMN recurring TO is_recurring_start
                        ", Context),
                    z_db:flush(Context),
                    ok;
                false ->
                    [] = z_db:q("
                        ALTER TABLE payment
                        ADD COLUMN is_recurring_start boolean not null default false
                        ", Context),
                    z_db:flush(Context),
                    ok
            end
    end.


%% @doc Add status_date column if it was not yet present
add_status_date_column(Context) ->
    case lists:member(status_date, z_db:column_names(payment, Context)) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                ALTER TABLE payment
                ADD COLUMN status_date timestamp with time zone
                ", Context),
            z_db:flush(Context),
            ok
    end.


cancel_recurring_payment(UserId, Context) ->
    z_db:q("update payment set is_recurring_start = false where user_id = $1", [UserId], Context).
