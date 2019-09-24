% Definitions for payment notifications

-define(PAYMENT_CURRENCY_DEFAULT, <<"EUR">>).

%% Some module wants to perform a payment request.
%% This is called with a 'first', returns a list of actions.
-record(payment_request, {
    % Data passed as identification of this request for status
    % payment notifications.
    key :: term(),
    user_id :: m_rsc:resource() | undefined,
    is_qargs = false :: boolean(),

    currency = ?PAYMENT_CURRENCY_DEFAULT :: binary(),
    amount :: float(),
    description = undefined :: binary() | undefined,
    description_html = undefined :: binary() | undefined,
    language :: atom(),
    recurring = false :: boolean(),
    extra_props = [] :: list( {atom(), binary()} )
}).

%% @doc On a successful payment initialization, return the uri where
%%      the user can finalize the payment.
-record(payment_request_redirect, {
    payment_id :: integer(),
    redirect_uri :: binary()
}).



%% Payment module notification, happens on payment status changes.
%% Listen to this if you want to do something when a payment status changes.
-record(payment_status, {
    key :: term(),
    payment_id :: integer(),
    user_id :: m_rsc:resource(),
    is_paid :: boolean(),
    is_failed :: boolean(),
    status :: binary()
}).




%% Notification for mod_payment, requesting a PSP to pick this up.
%% Returns a #payment_psp_handler{}
%%
-record(payment_psp_request, {
    payment_id :: integer(),
    payment_nr :: binary(),
    currency :: binary(),
    amount :: float(),
    recurring :: boolean()
}).

%% Notification for mod_payment, requesting a PSP to cancel a recurring payment.
%%
-record(cancel_subscription_psp_request, {
    user_id :: integer()
}).

%% Returned by the module handling the payment_psp_request
-record(payment_psp_handler, {
    psp_module :: atom(),
    psp_external_id :: binary(),
    psp_payment_description = <<>> :: binary(),
    psp_data :: term(),
    redirect_uri :: binary()
}).

%% Notification to fetch a detail url for a payment
-record(payment_psp_view_url, {
    payment_id :: integer(),
    psp_module :: atom(),
    psp_external_id :: binary(),
    psp_data :: term()
}).
