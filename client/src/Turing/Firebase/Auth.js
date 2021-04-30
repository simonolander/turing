"use strict";

exports.authImpl = function () {
    return firebase.auth();
};

exports.signInAnonymouslyImpl = function (auth) {
    return function () {
        return auth.signInAnonymously();
    };
};

exports.onAuthStateChangedImpl = function (auth) {
    return function (nextOrObserver) {
        return function () {
            auth.onAuthStateChanged(function (user) {
                nextOrObserver(user)();
            });
        };
    };
};
