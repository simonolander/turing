"use strict";

exports.auth = function () {
    return firebase.auth();
};

exports.signInAnonymously = function (auth) {
    return function () {
        return auth.signInAnonymously();
    };
};
