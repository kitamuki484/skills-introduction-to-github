! ============================================================
! 基本情報技術者試験 勉強時間管理アプリ
! Fundamental Information Technology Engineer Exam
! Study Time Tracker
!
! 目標: 最低40時間の勉強を達成する
! Goal : Reach at least 40 hours of study time
! ============================================================
program study_tracker
    implicit none

    integer, parameter :: TARGET_HOURS = 40
    integer, parameter :: MAX_SESSIONS = 1000
    integer, parameter :: LOG_UNIT = 10

    character(len=*), parameter :: LOG_FILE = 'study_log.dat'

    real :: session_hours(MAX_SESSIONS)
    character(len=20) :: session_dates(MAX_SESSIONS)
    integer :: num_sessions
    real :: total_hours
    integer :: choice
    logical :: running

    num_sessions = 0
    total_hours = 0.0
    running = .true.

    call load_log(LOG_UNIT, LOG_FILE, session_hours, session_dates, &
                  num_sessions, MAX_SESSIONS, total_hours)

    write(*, '(A)') '======================================================'
    write(*, '(A)') '  基本情報技術者試験 勉強時間管理アプリへようこそ！'
    write(*, '(A)') '  FE Exam Study Time Tracker'
    write(*, '(A)') '======================================================'
    write(*, *)

    do while (running)
        call show_menu()
        read(*, *, iostat=choice) choice

        select case (choice)
        case (1)
            call add_session(LOG_UNIT, LOG_FILE, session_hours, session_dates, &
                             num_sessions, MAX_SESSIONS, total_hours)
        case (2)
            call show_progress(session_hours, session_dates, num_sessions, &
                               total_hours, TARGET_HOURS)
        case (3)
            call show_summary(total_hours, TARGET_HOURS)
        case (0)
            write(*, '(A)') '  アプリを終了します。頑張ってください！'
            write(*, '(A)') '  Goodbye! Keep studying!'
            running = .false.
        case default
            write(*, '(A)') '  無効な選択です。もう一度入力してください。'
            write(*, '(A)') '  Invalid choice. Please try again.'
        end select

        write(*, *)
    end do

end program study_tracker

! ============================================================
! メニューを表示する
! ============================================================
subroutine show_menu()
    implicit none
    write(*, '(A)') '------------------------------------------------------'
    write(*, '(A)') '  メニュー / Menu'
    write(*, '(A)') '  1. 勉強時間を追加する / Add a study session'
    write(*, '(A)') '  2. 学習履歴を表示する / Show session history'
    write(*, '(A)') '  3. 進捗サマリーを表示する / Show progress summary'
    write(*, '(A)') '  0. 終了 / Exit'
    write(*, '(A)') '------------------------------------------------------'
    write(*, '(A)', advance='no') '  選択してください / Enter choice: '
end subroutine show_menu

! ============================================================
! 勉強セッションを追加する
! ============================================================
subroutine add_session(unit, filename, hours_arr, dates_arr, &
                       n, max_n, total)
    implicit none
    integer, intent(in) :: unit, max_n
    character(len=*), intent(in) :: filename
    real, intent(inout) :: hours_arr(max_n), total
    character(len=20), intent(inout) :: dates_arr(max_n)
    integer, intent(inout) :: n

    real :: h
    character(len=20) :: date_str
    integer :: ios

    if (n >= max_n) then
        write(*, '(A)') '  記録数が上限に達しました。/ Session limit reached.'
        return
    end if

    write(*, '(A)', advance='no') '  日付を入力してください (例: 2025-01-15) / Date (e.g. 2025-01-15): '
    read(*, '(A)', iostat=ios) date_str
    if (ios /= 0) then
        write(*, '(A)') '  入力エラー / Input error.'
        return
    end if

    write(*, '(A)', advance='no') '  勉強時間（時間単位）を入力してください / Study hours (e.g. 1.5): '
    read(*, *, iostat=ios) h
    if (ios /= 0 .or. h <= 0.0) then
        write(*, '(A)') '  無効な時間です。正の数値を入力してください。'
        write(*, '(A)') '  Invalid hours. Enter a positive number.'
        return
    end if

    n = n + 1
    hours_arr(n) = h
    dates_arr(n) = trim(date_str)
    total = total + h

    ! セッションをファイルに追記する / Append session to file
    open(unit=unit, file=filename, status='unknown', action='write', &
         position='append', iostat=ios)
    if (ios == 0) then
        write(unit, '(A20, F10.2)') trim(dates_arr(n)), hours_arr(n)
        close(unit)
    else
        write(*, '(A)') '  警告: ファイルへの書き込みに失敗しました。/ Warning: could not write to file.'
    end if

    write(*, '(A, F6.2, A)') '  追加しました！ / Added ', h, ' hours.'
    write(*, '(A, F7.2, A, I0, A)') '  累計: ', total, ' 時間 / Total: ', nint(total), ' hours recorded.'

end subroutine add_session

! ============================================================
! 学習履歴を表示する
! ============================================================
subroutine show_progress(hours_arr, dates_arr, n, total, target)
    implicit none
    integer, intent(in) :: n, target
    real, intent(in) :: hours_arr(n), total
    character(len=20), intent(in) :: dates_arr(n)
    integer :: i

    if (n == 0) then
        write(*, '(A)') '  まだ記録がありません。/ No sessions recorded yet.'
        return
    end if

    write(*, '(A)') '  --- 学習履歴 / Session History ---'
    write(*, '(A)') '  No.  日付 / Date          時間 / Hours'
    write(*, '(A)') '  ---  ----------------    -----'
    do i = 1, n
        write(*, '(2X, I3, 2X, A20, 2X, F6.2)') i, dates_arr(i), hours_arr(i)
    end do
    write(*, '(A)') '  -------------------------------------------'
    write(*, '(A, F7.2, A)') '  合計 / Total: ', total, ' 時間 / hours'
    call show_summary(total, target)

end subroutine show_progress

! ============================================================
! 進捗サマリーを表示する
! ============================================================
subroutine show_summary(total, target)
    implicit none
    real, intent(in) :: total
    integer, intent(in) :: target
    real :: remaining, pct
    integer :: bar_filled, i

    remaining = real(target) - total
    pct = min(total / real(target) * 100.0, 100.0)
    bar_filled = min(int(pct / 5.0), 20)  ! 20-char bar (each char = 5%)

    write(*, *)
    write(*, '(A)') '  ========== 進捗サマリー / Progress Summary =========='
    write(*, '(A, I0, A)')  '  目標 / Target      : ', target, ' 時間 / hours'
    write(*, '(A, F7.2, A)') '  累計 / Completed   : ', total, ' 時間 / hours'

    if (remaining > 0.0) then
        write(*, '(A, F7.2, A)') '  残り / Remaining   : ', remaining, ' 時間 / hours'
    else
        write(*, '(A)') '  残り / Remaining   :  0.00 時間 / hours  ✓ 達成済み!'
    end if

    write(*, '(A, F5.1, A)') '  達成率 / Progress  : ', pct, ' %'

    ! プログレスバー / Progress bar
    write(*, '(A)', advance='no') '  ['
    do i = 1, 20
        if (i <= bar_filled) then
            write(*, '(A)', advance='no') '#'
        else
            write(*, '(A)', advance='no') '-'
        end if
    end do
    write(*, '(A)') ']'

    write(*, *)
    if (total >= real(target)) then
        write(*, '(A)') '  ★★★ おめでとうございます！目標40時間を達成しました！★★★'
        write(*, '(A)') '  ★★★ Congratulations! You reached the 40-hour goal! ★★★'
    else
        write(*, '(A)') '  引き続き頑張ってください！/ Keep going, you can do it!'
    end if
    write(*, '(A)') '  ====================================================='

end subroutine show_summary

! ============================================================
! ログファイルから過去のセッションを読み込む
! ============================================================
subroutine load_log(unit, filename, hours_arr, dates_arr, &
                    n, max_n, total)
    implicit none
    integer, intent(in) :: unit, max_n
    character(len=*), intent(in) :: filename
    real, intent(inout) :: hours_arr(max_n), total
    character(len=20), intent(inout) :: dates_arr(max_n)
    integer, intent(inout) :: n

    real :: h
    character(len=20) :: d
    integer :: ios

    n = 0
    total = 0.0

    open(unit=unit, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) return  ! ファイルが存在しない場合はスキップ / No file yet

    do
        read(unit, '(A20, F10.2)', iostat=ios) d, h
        if (ios /= 0) exit
        if (n < max_n) then
            n = n + 1
            dates_arr(n) = trim(d)
            hours_arr(n) = h
            total = total + h
        end if
    end do

    close(unit)

    if (n > 0) then
        write(*, '(A, I0, A, F7.2, A)') &
            '  過去のデータを読み込みました: ', n, ' セッション, 累計 ', total, ' 時間'
        write(*, '(A, I0, A, F7.2, A)') &
            '  Loaded existing data: ', n, ' session(s), total ', total, ' hours'
        write(*, *)
    end if

end subroutine load_log
