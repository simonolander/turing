"use strict";

// See https://firebase.google.com/docs/reference/js/firebase.firestore.QuerySnapshot#docs
exports.docs = function (querySnapshot) {
    return querySnapshot.docs;
};
