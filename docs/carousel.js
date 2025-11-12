// Simple Carousel/Slider Implementation
document.addEventListener('DOMContentLoaded', function() {
  const sliders = document.querySelectorAll('.click-slider');
  
  sliders.forEach((slider, sliderIndex) => {
    const slides = slider.querySelectorAll('.slick-slide');
    let currentSlide = 0;
    
    if (slides.length === 0) return;
    
    // Initialize - show first slide
    slides[0].classList.add('slick-active');
    
    // Create navigation buttons
    const prevBtn = document.createElement('button');
    prevBtn.classList.add('slick-prev');
    prevBtn.innerHTML = '❮';
    prevBtn.setAttribute('aria-label', 'Previous quote');
    
    const nextBtn = document.createElement('button');
    nextBtn.classList.add('slick-next');
    nextBtn.innerHTML = '❯';
    nextBtn.setAttribute('aria-label', 'Next quote');
    
    slider.parentElement.appendChild(prevBtn);
    slider.parentElement.appendChild(nextBtn);
    
    // Create dots for pagination
    const dotsContainer = document.createElement('ul');
    dotsContainer.classList.add('slick-dots');
    
    slides.forEach((slide, index) => {
      const dot = document.createElement('li');
      const btn = document.createElement('button');
      btn.setAttribute('aria-label', `Go to quote ${index + 1}`);
      btn.onclick = (e) => {
        e.preventDefault();
        goToSlide(index);
      };
      dot.appendChild(btn);
      dotsContainer.appendChild(dot);
      
      if (index === 0) {
        dot.classList.add('slick-active');
      }
    });
    
    slider.parentElement.appendChild(dotsContainer);
    
    function showSlide(n) {
      slides[currentSlide].classList.remove('slick-active');
      document.querySelectorAll('.slick-dots li')[currentSlide].classList.remove('slick-active');
      
      currentSlide = (n + slides.length) % slides.length;
      
      slides[currentSlide].classList.add('slick-active');
      document.querySelectorAll('.slick-dots li')[currentSlide].classList.add('slick-active');
    }
    
    function goToSlide(n) {
      showSlide(n);
    }
    
    function nextSlide() {
      showSlide(currentSlide + 1);
    }
    
    function prevSlide() {
      showSlide(currentSlide - 1);
    }
    
    // Event listeners
    nextBtn.addEventListener('click', nextSlide);
    prevBtn.addEventListener('click', prevSlide);
    
    // Optional: Auto-rotate every 10 seconds
    setInterval(nextSlide, 10000);
  });
});
