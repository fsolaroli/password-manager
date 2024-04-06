'strict';

const _currentCommit = function() {
    return function() { return process.env.CURRENT_COMMIT };
}

const _shareURL = function() {
	return function() { return process.env.SHARE_URL };
}

const _redeemURL = function() {
	return function() { return process.env.REDEEM_URL };
}

const _appURL = function() {
	return function() { return process.env.APP_URL };
}

const _donationIFrameURL = function() {
	return function () { return process.env.DONATION_IFRAME_URL };
}

export { _currentCommit, _shareURL, _redeemURL, _appURL, _donationIFrameURL };
