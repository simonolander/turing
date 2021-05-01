"use strict";

exports.firestore = function () {
    return firebase.firestore();
};

exports.collection = function (firestore) {
    return function (collectionPath) {
        return function () {
            return firestore.collection(collectionPath);
        };
    };
};

exports.get = function (collectionReference) {
    return function (options) {
        return function () {
            return collectionReference.get(options);
        };
    };
};

exports.add = function (collectionReference) {
    return function (data) {
        return function () {
            return collectionReference.add(data);
        };
    };
};
