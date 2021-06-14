Payments module for Zotonic
===========================

Base module for handling payments. Uses Payment Service Provider (PSP) modules for interfacing with
the payment providers.


Payment form
------------

To add a payment form to your website, make a form with the following postback:

    {% wire id="mypayment" type="submit" postback={payment key="web-donation"} delegate=`mod_payment %}
    <form id="mypayment" method="post" action="postback">
        ...
    </form>

Optional arguments of the payment postback:

 * `key` - an identification of the payment type, also shown on the overview in the admin
 * `amount` - the amount to be paid, overrules the `amount` form field
 * `currency` - the currency for the amount, overrules `currency` form field (defaults to EUR)
 * `is_recurring_start` - if the payment is recurring, overrules `is_recurring_start` form field (defaults to false)
 * `user_id` - the id of the user the payment is for (defaults to the current user)
 * `description` - the description for the ordered goods/services (HTML)
 * `default_description` - used as the description if both `description` and `q.description` are empty
 * All other arguments are saved as additional properties, which can be overruled with form fields

Required fields for the payment form:

 * `amount` - if there is no amount in the postback, an integer of floating point number
 * `email` - email address (defaults to current user's)
 * `name_surname` - surname, required if there is no user_id (defaults to current user's)

Optional fields for the payment form:

 * `currency` - if there is no currency in the postback, the used currency, defaults to `EUR`
 * `is_recurring_start` - flag if the payment should be a recurring payment, defaults to `false` (see section below)
 * `name_first` - first name (defaults to current user's)
 * `phone` - phone number (defaults to current user's)
 * `address_street_1` - First line of address (defaults to current user's)
 * `address_street_2` - Second line of address (defaults to current user's)
 * `address_city` - City of address (defaults to current user's)
 * `address_state` - State of address (defaults to current user's)
 * `address_postcode` - Postcode of address (defaults to current user's)
 * `address_country` - Country of address, should be two letter ISO code (defaults to current user's)
 * `description` - Description of what has been ordered, could also be a referral identifier

Subscriptions / Recurring Payments
----------------------------------

Subscriptions are recurring payments.
All subscriptions *MUST* have an user_id attached.

The period of the recurring payment (monthly / yearly) is user-defined and should be explained
to the user in explanatory texts. The used PSP should know about this period as well.

The module `mod_payment` only initiates the first payment. Any subsequent payments should be initiated
by the used PSP.

Any user can have at most one (1) subscription.

The subscription can be canceled with the following postback:

    {% wire id='cancel-subscription-button'
            postback={cancel_subscription}
    %}


PSP Modules
-----------

This module provides the central payment administration. Interfacing with Payment Service Providers (PSP)
is done using PSP specific modules.

Current PSP specific module implements a payment interface to Mollie:

    https://github.com/driebit/mod_payment_mollie

Please check the modules for PSP specific configurations.


