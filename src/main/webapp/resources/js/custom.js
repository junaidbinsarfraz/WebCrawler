$( document ).ready(function() {
	stop();
	
	var mailme = function() {
	    console.log('Caught!');
	}

	window.addEventListener('error', function(e) {
	    /*var ie = window.event || {};
	    var errMsg = e.message || ie.errorMessage || "404 error on " + window.location;
	    var errSrc = (e.filename || ie.errorUrl) + ': ' + (e.lineno || ie.errorLine);
	    mailme([errMsg, errSrc]);*/
		stop();
	}, true);
	
});

function start() {
	$('.error').html('');
	$('.sendBtn').attr('disabled', 'disabled');
	if(PF('poll') && !PF('poll').isActive()){
		PF('poll').start();
	}
}

function stop() {
	if(PF('poll') && PF('poll').isActive()){
		PF('poll').stop();
	}
}

function error() {
	stop();
}