$( document ).ready(function() {
	stop();
	stopRemoval();
	
	window.addEventListener('error', function(e) {
		stop();
		stopRemoval();
	}, true);
	
});

function start() {
	$('.error').html('');
	$('.sendBtn').attr('disabled', 'disabled');
	if(PF('poll') && !PF('poll').isActive()){
		PF('poll').start();
	}
}

function startRemoval() {
	$('.errorRemoval').html('');
	$('.sendBtnRemoval').attr('disabled', 'disabled');
	if(PF('pollRemoval') && !PF('pollRemoval').isActive()){
		PF('pollRemoval').start();
	}
}

function stop() {
	if(PF('poll') && PF('poll').isActive()){
		PF('poll').stop();
	}
}

function stopRemoval() {
	if(PF('pollRemoval') && PF('pollRemoval').isActive()){
		PF('pollRemoval').stop();
	}
}

function error() {
	stop();
	stopRemoval();
}