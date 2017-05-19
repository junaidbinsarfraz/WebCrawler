$( document ).ready(function() {
	stop();
	
	window.addEventListener('error', function(e) {
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