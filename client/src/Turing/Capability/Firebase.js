"use strict";

exports.auth = function () {
    return firebase.auth();
};

exports.signInAnonymouslyImpl = function (auth) {
    return function () {
        return auth.signInAnonymously()
    };
};
