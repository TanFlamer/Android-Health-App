package com.example.myapp.mainActivities.login;

import android.annotation.SuppressLint;
import android.app.AlarmManager;
import android.app.AlertDialog;
import android.app.Application;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;

import com.example.myapp.MainApplication;
import com.example.myapp.Notification;
import com.example.myapp.databasefiles.user.User;
import com.example.myapp.databasefiles.user.UserRepository;
import com.example.myapp.mainActivities.account.AccountActivity;

import java.util.Calendar;

public class LoginViewModel extends AndroidViewModel {

    private final UserRepository userRepository;

    public LoginViewModel(@NonNull Application application) {
        super(application);
        MainApplication mainApplication = getApplication();
        userRepository = mainApplication.getUserRepository();
        createNotification();
    }

    public Intent validateUser(String username, String password){
        User user = userRepository.findUser(username);
        if(user != null && user.getPassword().equals(password))
            return loginUser(user);
        else{
            Toast.makeText(getApplication(), "Invalid Login Credentials", Toast.LENGTH_SHORT).show();
            return null;
        }
    }

    public Intent loginGuest(){
        Intent intent = new Intent(getApplication(), AccountActivity.class);
        intent.putExtra("username", "GUEST");
        return intent;
    }

    public AlertDialog guestDialog(Context context){
        return new AlertDialog.Builder(context)
                .setTitle("Guest Login")
                .setMessage("Are you sure you want to login as guest? A shared account is used.")
                .setPositiveButton("Yes", (dialogInterface, i) -> context.startActivity(loginGuest()))
                .setNegativeButton("No", null)
                .create();
    }

    public Intent loginNewUser(){
        Intent intent = new Intent(getApplication(), AccountActivity.class);
        intent.putExtra("username", "NEW USER");
        return intent;
    }

    public Intent loginUser(User user){
        Intent intent = new Intent(getApplication(), AccountActivity.class);
        intent.putExtra("username", user.getUsername());
        return intent;
    }

    @SuppressLint("UnspecifiedImmutableFlag")
    public void createNotification(){
        createNotificationChannel();
        Intent intent = new Intent(getApplication(), Notification.class);
        PendingIntent pendingIntent = PendingIntent.getBroadcast(getApplication(), 0, intent, PendingIntent.FLAG_IMMUTABLE);
        AlarmManager alarmManager = (AlarmManager) getApplication().getSystemService(Context.ALARM_SERVICE);
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.HOUR_OF_DAY, 22);
        calendar.set(Calendar.MINUTE, 0);
        alarmManager.set(AlarmManager.RTC_WAKEUP, calendar.getTimeInMillis(), pendingIntent);
    }

    private void createNotificationChannel(){
        String name = "Sleep Reminder Channel";
        String description = "Channel for Sleep Reminder";
        int importance = NotificationManager.IMPORTANCE_DEFAULT;
        NotificationChannel channel = new NotificationChannel("sleepReminder", name, importance);
        channel.setDescription(description);
        NotificationManager notificationManager = getApplication().getSystemService(NotificationManager.class);
        notificationManager.createNotificationChannel(channel);
    }
}
