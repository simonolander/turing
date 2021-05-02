"use strict";

exports.id = function (documentSnapshot) {
    return function () {
        return documentSnapshot.id
    }
}

exports.exists = function (documentSnapshot) {
    return function () {
        return documentSnapshot.exists
    }
}

exports._data = function (documentSnapshot) {
    return function (options) {
        return function () {
            // I found no great way to deal with undefined values,
            // so I'm converting undefined to null for the moment.
            var data = documentSnapshot.data(options);
            if (typeof data !== "undefined") {
                return data;
            }
            else {
                return null;
            }
        };
    };
};
